use crate::{
    ir::{BinLogicOp, BinMathOp, Comparison, Instruction},
    stack::Stack,
    typ::Type,
};
use anyhow::Result;
use codemap::Spanned;
use cranelift::prelude::{
    codegen::{
        ir::{Function, Inst, UserFuncName},
        Context,
    },
    isa::TargetIsa,
    settings,
    types::{I32, I8},
    AbiParam, Configurable, FunctionBuilder, FunctionBuilderContext,
    InstBuilder, IntCC, Signature, Value,
};
use cranelift_module::{DataContext, DataId, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use std::{collections::HashMap, fs::File, io::Write, path::Path, sync::Arc};

pub struct Options<'a> {
    pub target_triple: &'a str,
    pub out_path: &'a Path,
}

pub fn compile(
    program: &crate::typ::CheckedProgram,
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

    let function_signatures = program
        .functions()
        .iter()
        .map(|(name, function)| {
            let params = function
                .signature
                .parameters
                .iter()
                .map(|typ| match typ {
                    Type::Bool => I8,
                    Type::I32 => I32,
                    Type::Type => todo!(),
                })
                .map(AbiParam::new)
                .collect();
            let mut returns = function
                .signature
                .returns
                .iter()
                .map(|typ| match typ {
                    Type::Bool => I8,
                    Type::I32 => I32,
                    Type::Type => todo!(),
                })
                .map(AbiParam::new)
                .collect::<Vec<_>>();
            if *name == "main" {
                returns.push(AbiParam::new(I32));
            }

            (
                &**name,
                Signature {
                    params,
                    returns,
                    call_conv: isa.default_call_conv(),
                },
            )
        })
        .collect::<HashMap<_, _>>();
    let function_ids = function_signatures
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
        program,
        function_ids,
        function_signatures,
        stack: Vec::new(),
        isa,
        object_module,
        extern_functions: HashMap::new(),
        extern_function_signatures,
        strings: HashMap::new(),
    };
    compiler.compile()?;

    let mut data_ctx = DataContext::new();
    for (s, data_id) in &compiler.strings {
        data_ctx.define(s.as_bytes().into());
        compiler
            .object_module
            .define_data(*data_id, &data_ctx)
            .unwrap();
    }

    let object_bytes = compiler.object_module.finish().emit()?;
    let mut object_file = File::create(options.out_path)?;
    object_file.write_all(&object_bytes)?;

    Ok(())
}

struct Compiler<'a> {
    program: &'a crate::typ::CheckedProgram,
    function_signatures: HashMap<&'a str, Signature>,
    function_ids: HashMap<&'a str, FuncId>,
    stack: Vec<Value>,
    isa: Arc<dyn TargetIsa>,
    object_module: ObjectModule,
    extern_functions: HashMap<&'static str, FuncId>,
    extern_function_signatures: HashMap<&'static str, Signature>,
    strings: HashMap<&'static str, DataId>,
}

impl Stack for Compiler<'_> {
    type Item = Value;

    fn push(&mut self, element: Self::Item) {
        self.stack.push(element);
    }

    fn pop(&mut self) -> Self::Item {
        self.stack.pop().unwrap()
    }
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

    fn allocate_str(
        &mut self,
        s: &'static str,
        fb: &mut FunctionBuilder,
    ) -> Value {
        let data_id = *self.strings.entry(s).or_insert_with(|| {
            self.object_module
                .declare_anonymous_data(false, false)
                .unwrap()
        });
        let global_value =
            self.object_module.declare_data_in_func(data_id, fb.func);
        fb.ins().global_value(self.isa.pointer_type(), global_value)
    }

    fn compile(&mut self) -> Result<()> {
        let mut ctx = Context::new();
        let mut func_ctx = FunctionBuilderContext::new();

        for (name, function) in self.program.functions() {
            self.compile_function(name, function, &mut ctx, &mut func_ctx)?;
        }

        Ok(())
    }

    fn compile_function(
        &mut self,
        name: &str,
        function: &crate::typ::CheckedFunction,
        ctx: &mut Context,
        func_ctx: &mut FunctionBuilderContext,
    ) -> Result<()> {
        let signature = self.function_signatures[name].clone();
        let func_id = self.function_ids[name];
        ctx.clear();
        ctx.func =
            Function::with_name_signature(UserFuncName::default(), signature);

        let mut fb = FunctionBuilder::new(&mut ctx.func, func_ctx);
        let block = fb.create_block();
        fb.append_block_params_for_function_params(block);
        self.stack = fb.block_params(block).to_vec();
        fb.switch_to_block(block);
        fb.seal_block(block);

        for instruction in &*function.body {
            self.compile_instruction(instruction, &mut fb);
        }

        if name == "main" {
            // Exit code
            self.stack.push(fb.ins().iconst(I32, 0));
        }
        fb.ins().return_(&self.stack);

        fb.finalize();
        self.object_module.define_function(func_id, ctx)?;

        Ok(())
    }

    fn compile_instruction(
        &mut self,
        instruction: &Instruction,
        fb: &mut FunctionBuilder,
    ) {
        match instruction {
            Instruction::Call(name) => {
                let function = &self.program.functions()[&**name];
                let param_count = function.signature.parameters.len();
                let func_id = self.function_ids[&**name];
                let func_ref =
                    self.object_module.declare_func_in_func(func_id, fb.func);
                let inst = fb.ins().call(
                    func_ref,
                    &self.stack[self.stack.len() - param_count..],
                );
                self.stack.truncate(self.stack.len() - param_count);
                self.stack.extend(fb.inst_results(inst));
            }
            Instruction::Then(body) => self.compile_then(body, fb),
            Instruction::ThenElse(then, else_) => {
                self.compile_then_else(then, else_, fb);
            }
            Instruction::PushI32(number) => {
                self.stack.push(fb.ins().iconst(I32, i64::from(*number)));
            }
            Instruction::PushBool(b) => {
                self.stack.push(fb.ins().iconst(I8, i64::from(*b)));
            }
            Instruction::PushType(_) | Instruction::TypeOf => todo!(),
            Instruction::Print => {
                let n = self.pop();
                let fmt = self.allocate_str("%d\0", fb);
                self.call_extern("printf", &[fmt, n], fb);
            }
            Instruction::Println => {
                let n = self.pop();
                let fmt = self.allocate_str("%d\n\0", fb);
                self.call_extern("printf", &[fmt, n], fb);
            }
            Instruction::PrintChar => {
                let n = self.pop();
                self.call_extern("spkl_print_char", &[n], fb);
            }
            Instruction::BinMathOp(op) => {
                let b = self.pop();
                let a = self.pop();
                self.push(match op {
                    BinMathOp::Add => fb.ins().iadd(a, b),
                    BinMathOp::Sub => fb.ins().isub(a, b),
                    BinMathOp::Mul => fb.ins().imul(a, b),
                    BinMathOp::Div => fb.ins().sdiv(a, b),
                    BinMathOp::Rem => fb.ins().srem(a, b),
                    BinMathOp::SillyAdd => todo!(),
                });
            }
            Instruction::Comparison(comparison) => {
                let b = self.pop();
                let a = self.pop();
                self.push(fb.ins().icmp(
                    match comparison {
                        Comparison::Lt => IntCC::SignedLessThan,
                        Comparison::Le => IntCC::SignedLessThanOrEqual,
                        Comparison::Eq => IntCC::Equal,
                        Comparison::Ge => IntCC::SignedGreaterThanOrEqual,
                        Comparison::Gt => IntCC::SignedGreaterThan,
                    },
                    a,
                    b,
                ));
            }
            Instruction::Not => {
                let b = self.pop();
                self.push(fb.ins().bxor_imm(b, 1));
            }
            Instruction::BinLogicOp(op) => {
                let b = self.pop();
                let a = self.pop();
                self.push(match op {
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
                });
            }
            Instruction::Drop => {
                self.pop();
            }
            Instruction::Dup => self.dup(),
            Instruction::Swap => self.swap(),
            Instruction::Over => self.over(),
            Instruction::Nip => self.nip(),
            Instruction::Tuck => self.tuck(),
        }
    }

    fn compile_then(
        &mut self,
        body: &[Spanned<Instruction>],
        fb: &mut FunctionBuilder,
    ) {
        let then = fb.create_block();
        let after = fb.create_block();

        let condition = self.pop();
        fb.ins().brif(condition, then, &[], after, &self.stack);
        fb.seal_block(then);

        let params_after = self
            .stack
            .iter()
            .map(|&value| {
                fb.append_block_param(after, fb.func.dfg.value_type(value))
            })
            .collect();

        fb.switch_to_block(then);
        for instruction in body {
            self.compile_instruction(instruction, fb);
        }
        fb.ins().jump(after, &self.stack);
        fb.seal_block(after);

        fb.switch_to_block(after);
        self.stack = params_after;
    }

    fn compile_then_else(
        &mut self,
        then: &[Spanned<Instruction>],
        else_: &[Spanned<Instruction>],
        fb: &mut FunctionBuilder,
    ) {
        let then_block = fb.create_block();
        let else_block = fb.create_block();
        let after_block = fb.create_block();

        let condition = self.pop();
        fb.ins().brif(condition, then_block, &[], else_block, &[]);
        fb.seal_block(then_block);
        fb.seal_block(else_block);

        let params_before = self.stack.clone();
        fb.switch_to_block(then_block);
        for instruction in then {
            self.compile_instruction(instruction, fb);
        }
        let params_after = self
            .stack
            .iter()
            .map(|&value| {
                fb.append_block_param(
                    after_block,
                    fb.func.dfg.value_type(value),
                )
            })
            .collect();
        fb.ins().jump(after_block, &self.stack);

        fb.switch_to_block(else_block);
        self.stack = params_before;
        for instruction in else_ {
            self.compile_instruction(instruction, fb);
        }
        fb.ins().jump(after_block, &self.stack);
        fb.seal_block(after_block);

        fb.switch_to_block(after_block);
        self.stack = params_after;
    }
}

fn extern_function_signatures(
    isa: &dyn TargetIsa,
) -> HashMap<&'static str, Signature> {
    let call_conv = isa.default_call_conv();
    let pointer = isa.pointer_type();

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
            "printf",
            Signature {
                params: vec![AbiParam::new(pointer), AbiParam::new(I32)],
                returns: vec![AbiParam::new(I32)],
                call_conv,
            },
        ),
    ])
}
