use crate::{
    ir::{BinLogicOp, BinMathOp, Comparison},
    ssa::{self, Op},
    typ::{FunctionSignature, Type},
};
use anyhow::Result;
use cranelift::prelude::{
    codegen::{
        ir::{Function, Inst, UserFuncName},
        Context,
    },
    isa::TargetIsa,
    settings,
    types::{F32, I32, I8},
    AbiParam, Configurable, FunctionBuilder, FunctionBuilderContext,
    InstBuilder, IntCC, MemFlags, Signature, StackSlotData, StackSlotKind,
    Value,
};
use cranelift_module::{FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use std::{collections::HashMap, fs::File, io::Write, path::Path};

pub struct Options<'a> {
    pub target_triple: &'a str,
    pub out_path: &'a Path,
}

pub fn compile(program: &ssa::Program, options: &Options) -> Result<()> {
    let mut shared_builder = settings::builder();
    shared_builder.enable("is_pic")?;
    shared_builder.set("opt_level", "speed_and_size")?;

    let shared_flags = settings::Flags::new(shared_builder);
    let isa = cranelift::codegen::isa::lookup_by_name(options.target_triple)?
        .finish(shared_flags)?;
    let extern_function_signatures = extern_function_signatures(&*isa);

    let object_builder = ObjectBuilder::new(
        isa.clone(),
        [],
        cranelift_module::default_libcall_names(),
    )?;
    let mut object_module = ObjectModule::new(object_builder);

    let clif_function_signatures = program
        .function_signatures
        .iter()
        .map(|(name, signature)| (&**name, signature.to_clif(name, &*isa)))
        .collect::<HashMap<_, _>>();
    let function_ids = clif_function_signatures
        .iter()
        .map(|(&name, signature)| {
            let func_id = if name == "main" {
                object_module.declare_function(
                    "main",
                    Linkage::Export,
                    signature,
                )
            } else {
                object_module.declare_anonymous_function(signature)
            }
            .unwrap();
            (name.clone(), func_id)
        })
        .collect();

    let mut compiler = Compiler {
        function_ids,
        clif_function_signatures,
        ssa_values: HashMap::new(),
        isa: &*isa,
        object_module,
        extern_functions: HashMap::new(),
        extern_function_signatures,
    };
    compiler.compile(program)?;

    let object_bytes = compiler.object_module.finish().emit()?;
    let mut object_file = File::create(options.out_path)?;
    object_file.write_all(&object_bytes)?;

    Ok(())
}

struct Compiler<'a> {
    clif_function_signatures: HashMap<&'a str, Signature>,
    function_ids: HashMap<&'a str, FuncId>,
    ssa_values: HashMap<ssa::Value, Value>,
    isa: &'a dyn TargetIsa,
    object_module: ObjectModule,
    extern_functions: HashMap<&'static str, FuncId>,
    extern_function_signatures: HashMap<&'static str, Signature>,
}

impl Compiler<'_> {
    fn take(&mut self, value: ssa::Value) -> Value {
        self.ssa_values
            .remove(&value)
            .unwrap_or_else(|| panic!("{value:?} is not defined"))
    }

    fn set(&mut self, value: ssa::Value, clif_value: Value) {
        self.ssa_values.insert(value, clif_value);
    }

    fn call_extern(
        &mut self,
        func_name: &'static str,
        args: &[Value],
        fb: &mut FunctionBuilder,
    ) -> Inst {
        let func_id =
            *self.extern_functions.entry(func_name).or_insert_with(|| {
                let Some(signature) = self.extern_function_signatures.get(func_name) else {
                    panic!("extern function `{func_name}` missing signature");
                };
                self.object_module
                    .declare_function(func_name, Linkage::Import, signature)
                    .unwrap()
            });
        let func_ref =
            self.object_module.declare_func_in_func(func_id, fb.func);
        fb.ins().call(func_ref, args)
    }

    fn compile(&mut self, program: &ssa::Program) -> Result<()> {
        let mut ctx = Context::new();
        let mut func_ctx = FunctionBuilderContext::new();

        for (name, body) in &program.function_bodies {
            self.compile_function(name, body, &mut ctx, &mut func_ctx)?;
        }

        Ok(())
    }

    fn compile_function(
        &mut self,
        name: &str,
        body: &ssa::Graph,
        ctx: &mut Context,
        func_ctx: &mut FunctionBuilderContext,
    ) -> Result<()> {
        let signature = self.clif_function_signatures[name].clone();
        let func_id = self.function_ids[name];
        ctx.clear();
        ctx.func =
            Function::with_name_signature(UserFuncName::default(), signature);

        let mut fb = FunctionBuilder::new(&mut ctx.func, func_ctx);
        let block = fb.create_block();
        fb.append_block_params_for_function_params(block);
        for (&ssa_value, &param) in
            std::iter::zip(&body.inputs, fb.block_params(block))
        {
            self.set(ssa_value, param);
        }
        fb.switch_to_block(block);
        fb.seal_block(block);

        for assignment in &body.assignments {
            self.compile_assignment(assignment, &mut fb);
        }

        let outputs = body
            .outputs
            .iter()
            .map(|output| self.ssa_values[output])
            // Exit code
            .chain((name == "main").then(|| fb.ins().iconst(I32, 0)))
            .collect::<Vec<_>>();
        fb.ins().return_(&outputs);

        fb.finalize();
        self.object_module.define_function(func_id, ctx)?;

        Ok(())
    }

    fn compile_assignment(
        &mut self,
        assignment: &ssa::Assignment,
        fb: &mut FunctionBuilder,
    ) {
        let to = assignment.to;
        let args = &assignment.args;
        match &assignment.op {
            Op::Call(name) => {
                let func_id = self.function_ids[&**name];
                let func_ref =
                    self.object_module.declare_func_in_func(func_id, fb.func);
                let call_args =
                    args.iter().map(|&arg| self.take(arg)).collect::<Vec<_>>();
                let inst = fb.ins().call(func_ref, &call_args);
                for (value, &res) in std::iter::zip(to, fb.inst_results(inst)) {
                    self.set(value, res);
                }
            }
            Op::Then(body) => self.compile_then(to, args, body, fb),
            Op::ThenElse(then, else_) => {
                self.compile_then_else(to, args, then, else_, fb);
            }
            Op::Repeat(body) => self.compile_repeat(to, args, body, fb),
            Op::Dup => {
                let v = self.take(args[0]);
                self.ssa_values.insert(to + 0, v);
                self.ssa_values.insert(to + 1, v);
            }
            Op::Drop => {
                self.take(args[0]);
            }
            Op::I32(number) => {
                self.set(to + 0, fb.ins().iconst(I32, i64::from(*number)));
            }
            Op::F32(number) => {
                self.set(to + 0, fb.ins().f32const(*number));
            }
            Op::Bool(b) => {
                self.set(to + 0, fb.ins().iconst(I8, i64::from(*b)));
            }
            Op::Type(_) | Op::TypeOf | Op::Ptr => todo!(),
            Op::PrintChar => {
                let n = self.take(args[0]);
                self.call_extern("spkl_print_char", &[n], fb);
            }
            Op::PrintI32 => {
                let n = self.take(args[0]);
                self.call_extern("spkl_print_i32", &[n], fb);
            }
            Op::PrintF32 => {
                let n = self.take(args[0]);
                self.call_extern("spkl_print_f32", &[n], fb);
            }
            Op::PrintlnI32 => {
                let n = self.take(args[0]);
                self.call_extern("spkl_println_i32", &[n], fb);
            }
            Op::PrintlnF32 => {
                let n = self.take(args[0]);
                self.call_extern("spkl_println_f32", &[n], fb);
            }
            Op::BinMath { operation, typ } => {
                let a = self.take(args[0]);
                let b = self.take(args[1]);
                self.set(
                    to + 0,
                    match (operation, typ) {
                        (BinMathOp::Add, Some(Type::I32)) => {
                            fb.ins().iadd(a, b)
                        }
                        (BinMathOp::Sub, Some(Type::I32)) => {
                            fb.ins().isub(a, b)
                        }
                        (BinMathOp::Mul, Some(Type::I32)) => {
                            fb.ins().imul(a, b)
                        }
                        (BinMathOp::Div, Some(Type::I32)) => {
                            fb.ins().sdiv(a, b)
                        }
                        (BinMathOp::Rem, _) => fb.ins().srem(a, b),
                        (BinMathOp::SillyAdd, _) => todo!(),
                        (BinMathOp::Add, Some(Type::F32)) => {
                            fb.ins().fadd(a, b)
                        }
                        (BinMathOp::Sub, Some(Type::F32)) => {
                            fb.ins().fsub(a, b)
                        }
                        (BinMathOp::Mul, Some(Type::F32)) => {
                            fb.ins().fmul(a, b)
                        }
                        (BinMathOp::Div, Some(Type::F32)) => {
                            fb.ins().fdiv(a, b)
                        }
                        _ => unreachable!(),
                    },
                );
            }
            Op::Sqrt => {
                let n = self.take(args[0]);
                self.set(to + 0, fb.ins().sqrt(n));
            }
            Op::Compare(comparison) => {
                let a = self.take(args[0]);
                let b = self.take(args[1]);
                self.set(
                    to + 0,
                    fb.ins().icmp(
                        match comparison {
                            Comparison::Lt => IntCC::SignedLessThan,
                            Comparison::Le => IntCC::SignedLessThanOrEqual,
                            Comparison::Eq => IntCC::Equal,
                            Comparison::Ge => IntCC::SignedGreaterThanOrEqual,
                            Comparison::Gt => IntCC::SignedGreaterThan,
                        },
                        a,
                        b,
                    ),
                );
            }
            Op::Not => {
                let b = self.take(args[0]);
                self.set(to + 0, fb.ins().bxor_imm(b, 1));
            }
            Op::BinLogic(op) => {
                let a = self.take(args[0]);
                let b = self.take(args[1]);
                self.set(
                    to + 0,
                    match op {
                        BinLogicOp::And => fb.ins().band(a, b),
                        BinLogicOp::Or => fb.ins().bor(a, b),
                        BinLogicOp::Xor => fb.ins().bxor(a, b),
                        BinLogicOp::Nand => {
                            let res = fb.ins().band(a, b);
                            fb.ins().bxor_imm(res, 1)
                        }
                        BinLogicOp::Nor => {
                            let res = fb.ins().bor(a, b);
                            fb.ins().bxor_imm(res, 1)
                        }
                        BinLogicOp::Xnor => {
                            let res = fb.ins().bxor(a, b);
                            fb.ins().bxor_imm(res, 1)
                        }
                    },
                );
            }
            Op::AddrOf(typ) => {
                let typ = typ.to_clif(self.isa).unwrap();
                let stack_slot = fb.create_sized_stack_slot(StackSlotData {
                    kind: StackSlotKind::ExplicitSlot,
                    size: typ.bytes(),
                });
                let v = self.take(args[0]);
                self.set(to + 0, v);
                fb.ins().stack_store(v, stack_slot, 0);
                self.set(
                    to + 1,
                    fb.ins().stack_addr(self.isa.pointer_type(), stack_slot, 0),
                );
            }
            Op::ReadPtr(typ) => {
                let ptr = self.take(args[0]);
                let typ = typ.to_clif(self.isa).unwrap();
                self.set(
                    to + 0,
                    fb.ins().load(typ, MemFlags::trusted(), ptr, 0),
                );
            }
        }
    }

    fn compile_then(
        &mut self,
        to: ssa::ValueSequence,
        args: &[ssa::Value],
        body: &ssa::Graph,
        fb: &mut FunctionBuilder,
    ) {
        let (&condition, args) = args.split_last().unwrap();

        for (arg, &input) in std::iter::zip(args, &body.inputs) {
            let clif_value = self.ssa_values[arg];
            self.set(input, clif_value);
        }

        let then = fb.create_block();
        let after = fb.create_block();

        let condition = self.take(condition);
        fb.ins().brif(
            condition,
            then,
            &[],
            after,
            &args.iter().map(|&arg| self.take(arg)).collect::<Vec<_>>(),
        );
        fb.seal_block(then);

        fb.switch_to_block(then);
        for assignment in &body.assignments {
            self.compile_assignment(assignment, fb);
        }
        for (value, out) in std::iter::zip(to, &body.outputs) {
            self.set(
                value,
                fb.append_block_param(
                    after,
                    fb.func.dfg.value_type(self.ssa_values[out]),
                ),
            );
        }
        fb.ins().jump(
            after,
            &body
                .outputs
                .iter()
                .map(|&out| self.take(out))
                .collect::<Vec<_>>(),
        );
        fb.seal_block(after);

        fb.switch_to_block(after);
    }

    fn compile_then_else(
        &mut self,
        to: ssa::ValueSequence,
        args: &[ssa::Value],
        then: &ssa::Graph,
        else_: &ssa::Graph,
        fb: &mut FunctionBuilder,
    ) {
        let (&condition, args) = args.split_last().unwrap();

        for (arg, &input) in std::iter::zip(args, &then.inputs) {
            self.set(input, self.ssa_values[arg]);
        }
        for (&arg, &input) in std::iter::zip(args, &else_.inputs) {
            let clif_value = self.take(arg);
            self.set(input, clif_value);
        }

        let then_block = fb.create_block();
        let else_block = fb.create_block();
        let after_block = fb.create_block();

        let condition = self.take(condition);
        fb.ins().brif(condition, then_block, &[], else_block, &[]);
        fb.seal_block(then_block);
        fb.seal_block(else_block);

        fb.switch_to_block(then_block);
        for assignment in &then.assignments {
            self.compile_assignment(assignment, fb);
        }
        for (value, out) in std::iter::zip(to, &then.outputs) {
            let v = self.ssa_values[out];
            self.set(
                value,
                fb.append_block_param(after_block, fb.func.dfg.value_type(v)),
            );
        }
        fb.ins().jump(
            after_block,
            &then
                .outputs
                .iter()
                .map(|&out| self.take(out))
                .collect::<Vec<_>>(),
        );

        fb.switch_to_block(else_block);
        for assignment in &else_.assignments {
            self.compile_assignment(assignment, fb);
        }
        fb.ins().jump(
            after_block,
            &else_
                .outputs
                .iter()
                .map(|&out| self.take(out))
                .collect::<Vec<_>>(),
        );
        fb.seal_block(after_block);

        fb.switch_to_block(after_block);
    }

    fn compile_repeat(
        &mut self,
        to: ssa::ValueSequence,
        args: &[ssa::Value],
        body: &ssa::Graph,
        fb: &mut FunctionBuilder,
    ) {
        let loop_block = fb.create_block();
        let after_block = fb.create_block();

        for (arg, &input) in std::iter::zip(args, &body.inputs) {
            let v = self.ssa_values[arg];
            self.set(
                input,
                fb.append_block_param(loop_block, fb.func.dfg.value_type(v)),
            );
        }

        fb.ins().jump(
            loop_block,
            &args.iter().map(|&arg| self.take(arg)).collect::<Vec<_>>(),
        );
        fb.switch_to_block(loop_block);
        for assignment in &body.assignments {
            self.compile_assignment(assignment, fb);
        }
        let (&condition, outputs) = body.outputs.split_last().unwrap();
        for (value, out) in std::iter::zip(to, outputs) {
            self.set(value, self.ssa_values[out]);
        }
        fb.ins().brif(
            self.take(condition),
            loop_block,
            &outputs
                .iter()
                .map(|&out| self.take(out))
                .collect::<Vec<_>>(),
            after_block,
            &[],
        );
        fb.seal_block(loop_block);
        fb.seal_block(after_block);

        fb.switch_to_block(after_block);
    }
}

fn extern_function_signatures(
    isa: &dyn TargetIsa,
) -> HashMap<&'static str, Signature> {
    let call_conv = isa.default_call_conv();

    HashMap::from([
        (
            "spkl_print_char",
            Signature {
                params: vec![AbiParam::new(I32)],
                returns: Vec::new(),
                call_conv,
            },
        ),
        (
            "spkl_print_i32",
            Signature {
                params: vec![AbiParam::new(I32)],
                returns: Vec::new(),
                call_conv,
            },
        ),
        (
            "spkl_println_i32",
            Signature {
                params: vec![AbiParam::new(I32)],
                returns: Vec::new(),
                call_conv,
            },
        ),
        (
            "spkl_print_f32",
            Signature {
                params: vec![AbiParam::new(F32)],
                returns: Vec::new(),
                call_conv,
            },
        ),
        (
            "spkl_println_f32",
            Signature {
                params: vec![AbiParam::new(F32)],
                returns: Vec::new(),
                call_conv,
            },
        ),
    ])
}

impl Type {
    fn to_clif(&self, isa: &dyn TargetIsa) -> Option<cranelift::prelude::Type> {
        Some(match self {
            Self::Bool => I8,
            Self::I32 => I32,
            Self::F32 => F32,
            Self::Type => return None,
            Self::Ptr(_) => isa.pointer_type(),
        })
    }
}

impl FunctionSignature {
    fn to_clif(&self, name: &str, isa: &dyn TargetIsa) -> Signature {
        let params = self
            .parameters
            .iter()
            .map(|typ| AbiParam::new(typ.to_clif(isa).unwrap()))
            .collect();
        let mut returns = self
            .returns
            .iter()
            .map(|typ| AbiParam::new(typ.to_clif(isa).unwrap()))
            .collect::<Vec<_>>();
        if name == "main" {
            returns.push(AbiParam::new(I32));
        }

        Signature {
            params,
            returns,
            call_conv: isa.default_call_conv(),
        }
    }
}
