use crate::{
    call_graph::CallGraph,
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
use std::{collections::BTreeMap, fs, path::Path};

pub struct Options<'a> {
    pub target_triple: &'a str,
    pub out_path: &'a Path,
}

pub fn compile(
    functions: &CallGraph,
    function_signatures: &BTreeMap<&str, FunctionSignature>,
    options: &Options,
) -> Result<()> {
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

    let clif_function_signatures = function_signatures
        .iter()
        .map(|(name, signature)| (&**name, signature.to_clif(name, &*isa)))
        .collect::<BTreeMap<_, _>>();
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
            (name, func_id)
        })
        .collect();

    let mut compiler = Compiler {
        function_ids,
        clif_function_signatures,
        isa: &*isa,
        object_module,
        extern_functions: BTreeMap::new(),
        extern_function_signatures,
    };
    compiler.compile(functions)?;

    let object_bytes = compiler.object_module.finish().emit()?;
    fs::write(options.out_path, &object_bytes)?;

    Ok(())
}

struct Compiler<'a> {
    clif_function_signatures: BTreeMap<&'a str, Signature>,
    function_ids: BTreeMap<&'a str, FuncId>,
    isa: &'a dyn TargetIsa,
    object_module: ObjectModule,
    extern_functions: BTreeMap<&'static str, FuncId>,
    extern_function_signatures: BTreeMap<&'static str, Signature>,
}

impl Compiler<'_> {
    fn call_extern(
        &mut self,
        func_name: &'static str,
        args: &[Value],
        fb: &mut FunctionBuilder,
    ) -> Inst {
        let func_id =
            *self.extern_functions.entry(func_name).or_insert_with(|| {
                let Some(signature) =
                    self.extern_function_signatures.get(func_name)
                else {
                    panic!("extern function `{func_name}` missing signature");
                };
                self.object_module
                    .declare_function(func_name, Linkage::Hidden, signature)
                    .unwrap()
            });
        let func_ref =
            self.object_module.declare_func_in_func(func_id, fb.func);
        fb.ins().call(func_ref, args)
    }

    fn compile(&mut self, functions: &CallGraph) -> Result<()> {
        let mut ctx = Context::new();
        let mut func_ctx = FunctionBuilderContext::new();

        for function in functions.node_weights() {
            self.compile_function(
                function.name,
                &function.body,
                &mut ctx,
                &mut func_ctx,
            )?;
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
        fb.switch_to_block(block);
        fb.seal_block(block);

        let inputs = Box::from(fb.block_params(block));
        let mut outputs = self.compile_graph(body, &inputs, &mut fb);

        if name == "main" {
            // Exit code
            outputs.push(fb.ins().iconst(I32, 0));
        }

        fb.ins().return_(&outputs);

        fb.finalize();
        self.object_module.define_function(func_id, ctx)?;

        Ok(())
    }

    fn compile_graph(
        &mut self,
        graph: &ssa::Graph,
        inputs: &[Value],
        fb: &mut FunctionBuilder,
    ) -> Vec<Value> {
        let mut clif_values = graph
            .defs
            .iter()
            .filter_map(|(&var, def)| match *def {
                ssa::Def::Input { index } => Some((var, inputs[index])),
                ssa::Def::Op { .. } => None,
            })
            .collect::<BTreeMap<_, _>>();

        for (op_index, op) in graph.ops.iter().enumerate() {
            let args_by_index = graph
                .uses
                .iter()
                .filter_map(|(var, use_)| match *use_ {
                    ssa::Use::Op { index, sub_index } if index == op_index => {
                        Some((sub_index, clif_values[var]))
                    }
                    _ => None,
                })
                .collect::<BTreeMap<_, _>>();
            let args = args_by_index.values().copied().collect::<Box<_>>();

            let outputs = self.compile_op(op, &args, fb);

            clif_values.extend(graph.defs.iter().filter_map(|(&var, def)| {
                match *def {
                    ssa::Def::Op { index, sub_index } if index == op_index => {
                        Some((var, outputs[sub_index]))
                    }
                    _ => None,
                }
            }));
        }

        let outputs_by_index = graph
            .uses
            .iter()
            .filter_map(|(var, use_)| match *use_ {
                ssa::Use::Output { index } => Some((index, clif_values[var])),
                ssa::Use::Op { .. } => None,
            })
            .collect::<BTreeMap<_, _>>();
        outputs_by_index.values().copied().collect()
    }

    fn compile_op(
        &mut self,
        op: &ssa::Op,
        args: &[Value],
        fb: &mut FunctionBuilder,
    ) -> Vec<Value> {
        match op {
            Op::Call(name) => {
                let func_id = self.function_ids[&**name];
                let func_ref =
                    self.object_module.declare_func_in_func(func_id, fb.func);
                let inst = fb.ins().call(func_ref, args);
                fb.inst_results(inst).into()
            }
            Op::Then(body) => self.compile_then(args, body, fb),
            Op::ThenElse(then, else_) => {
                self.compile_then_else(args, then, else_, fb)
            }
            Op::Repeat(body) => self.compile_repeat(args, body, fb),
            Op::Dup => args.repeat(2),
            Op::Drop => Vec::new(),
            Op::I32(number) => {
                Vec::from([fb.ins().iconst(I32, i64::from(*number))])
            }
            Op::F32(number) => Vec::from([fb.ins().f32const(*number)]),
            Op::Bool(b) => Vec::from([fb.ins().iconst(I8, i64::from(*b))]),
            Op::Type | Op::TypeOf | Op::Ptr => todo!(),
            Op::PrintChar => {
                self.call_extern("spkl_print_char", args, fb);
                Vec::new()
            }
            Op::PrintI32 => {
                self.call_extern("spkl_print_i32", args, fb);
                Vec::new()
            }
            Op::PrintF32 => {
                self.call_extern("spkl_print_f32", args, fb);
                Vec::new()
            }
            Op::PrintlnI32 => {
                self.call_extern("spkl_println_i32", args, fb);
                Vec::new()
            }
            Op::PrintlnF32 => {
                self.call_extern("spkl_println_f32", args, fb);
                Vec::new()
            }
            Op::BinMath { operation, typ } => {
                let a = args[0];
                let b = args[1];
                let res = match (operation, typ) {
                    (BinMathOp::Add, Some(Type::I32)) => fb.ins().iadd(a, b),
                    (BinMathOp::Sub, Some(Type::I32)) => fb.ins().isub(a, b),
                    (BinMathOp::Mul, Some(Type::I32)) => fb.ins().imul(a, b),
                    (BinMathOp::Div, Some(Type::I32)) => fb.ins().sdiv(a, b),
                    (BinMathOp::Rem, _) => fb.ins().srem(a, b),
                    (BinMathOp::SillyAdd, _) => todo!(),
                    (BinMathOp::Add, Some(Type::F32)) => fb.ins().fadd(a, b),
                    (BinMathOp::Sub, Some(Type::F32)) => fb.ins().fsub(a, b),
                    (BinMathOp::Mul, Some(Type::F32)) => fb.ins().fmul(a, b),
                    (BinMathOp::Div, Some(Type::F32)) => fb.ins().fdiv(a, b),
                    _ => unreachable!(),
                };
                Vec::from([res])
            }
            Op::Sqrt => Vec::from([fb.ins().sqrt(args[0])]),
            Op::Compare(comparison) => {
                let res = fb.ins().icmp(
                    match comparison {
                        Comparison::Lt => IntCC::SignedLessThan,
                        Comparison::Le => IntCC::SignedLessThanOrEqual,
                        Comparison::Eq => IntCC::Equal,
                        Comparison::Ge => IntCC::SignedGreaterThanOrEqual,
                        Comparison::Gt => IntCC::SignedGreaterThan,
                    },
                    args[0],
                    args[1],
                );
                Vec::from([res])
            }
            Op::Not => Vec::from([fb.ins().bxor_imm(args[0], 1)]),
            Op::BinLogic(op) => {
                let a = args[0];
                let b = args[1];
                let res = match op {
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
                };
                Vec::from([res])
            }
            Op::AddrOf(typ) => {
                let typ = typ.to_clif(self.isa).unwrap();
                let stack_slot = fb.create_sized_stack_slot(StackSlotData {
                    kind: StackSlotKind::ExplicitSlot,
                    size: typ.bytes(),
                });
                let v = args[0];
                fb.ins().stack_store(v, stack_slot, 0);
                let res =
                    fb.ins().stack_addr(self.isa.pointer_type(), stack_slot, 0);
                Vec::from([v, res])
            }
            Op::ReadPtr(typ) => {
                let typ = typ.to_clif(self.isa).unwrap();
                let res = fb.ins().load(typ, MemFlags::trusted(), args[0], 0);
                Vec::from([res])
            }
        }
    }

    fn compile_then(
        &mut self,
        args: &[Value],
        body: &ssa::Graph,
        fb: &mut FunctionBuilder,
    ) -> Vec<Value> {
        let (&condition, args) = args.split_last().unwrap();

        let then = fb.create_block();
        let after = fb.create_block();

        let results = args
            .iter()
            .map(|&arg| {
                fb.append_block_param(after, fb.func.dfg.value_type(arg))
            })
            .collect();

        fb.ins().brif(condition, then, &[], after, args);
        fb.seal_block(then);

        fb.switch_to_block(then);
        let body_outputs = self.compile_graph(body, args, fb);
        fb.ins().jump(after, &body_outputs);
        fb.seal_block(after);

        fb.switch_to_block(after);

        results
    }

    fn compile_then_else(
        &mut self,
        args: &[Value],
        then: &ssa::Graph,
        else_: &ssa::Graph,
        fb: &mut FunctionBuilder,
    ) -> Vec<Value> {
        let (&condition, args) = args.split_last().unwrap();

        let then_block = fb.create_block();
        let else_block = fb.create_block();
        let after_block = fb.create_block();

        fb.ins().brif(condition, then_block, &[], else_block, &[]);
        fb.seal_block(then_block);
        fb.seal_block(else_block);

        fb.switch_to_block(then_block);
        let then_outputs = self.compile_graph(then, args, fb);
        let results = then_outputs
            .iter()
            .map(|&output| {
                fb.append_block_param(
                    after_block,
                    fb.func.dfg.value_type(output),
                )
            })
            .collect();
        fb.ins().jump(after_block, &then_outputs);

        fb.switch_to_block(else_block);
        let else_outputs = self.compile_graph(else_, args, fb);
        fb.ins().jump(after_block, &else_outputs);
        fb.seal_block(after_block);

        fb.switch_to_block(after_block);

        results
    }

    fn compile_repeat(
        &mut self,
        args: &[Value],
        body: &ssa::Graph,
        fb: &mut FunctionBuilder,
    ) -> Vec<Value> {
        let loop_block = fb.create_block();
        let after_block = fb.create_block();

        let loop_args = args
            .iter()
            .map(|&arg| {
                fb.append_block_param(loop_block, fb.func.dfg.value_type(arg))
            })
            .collect::<Box<_>>();

        fb.ins().jump(loop_block, args);
        fb.switch_to_block(loop_block);
        let mut loop_outputs = self.compile_graph(body, &loop_args, fb);
        let condition = loop_outputs.pop().unwrap();
        fb.ins()
            .brif(condition, loop_block, &loop_outputs, after_block, &[]);
        fb.seal_block(loop_block);
        fb.seal_block(after_block);

        fb.switch_to_block(after_block);

        loop_outputs
    }
}

fn extern_function_signatures(
    isa: &dyn TargetIsa,
) -> BTreeMap<&'static str, Signature> {
    let call_conv = isa.default_call_conv();

    BTreeMap::from([
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
