use crate::{
    ir::{BinLogicOp, BinMathOp, Comparison, Instruction, Program},
    stack::Stack,
};
use anyhow::Result;
use cranelift::prelude::{
    codegen::{
        ir::{Function, Inst, UserFuncName},
        Context,
    },
    isa::TargetIsa,
    settings,
    types::{I32, I8},
    AbiParam, Configurable, FunctionBuilder, FunctionBuilderContext,
    InstBuilder, IntCC, Signature, Type, Value,
};
use cranelift_module::{DataContext, DataId, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use std::{collections::HashMap, fs::File, io::Write, path::Path};

pub struct Options<'a> {
    pub target_triple: &'a str,
    pub out_path: &'a Path,
}

pub fn compile(
    program: &crate::typ::Checked<Program>,
    options: &Options,
) -> Result<()> {
    let mut shared_builder = settings::builder();
    shared_builder.enable("is_pic")?;
    shared_builder.set("opt_level", "speed_and_size")?;

    let shared_flags = settings::Flags::new(shared_builder);
    let isa = cranelift::codegen::isa::lookup_by_name(options.target_triple)?
        .finish(shared_flags)?;
    let call_conv = isa.default_call_conv();
    let pointer_type = isa.pointer_type();
    let extern_function_signatures = extern_function_signatures(&*isa);

    let mut ctx = Context::new();

    let object_builder =
        ObjectBuilder::new(isa, [], cranelift_module::default_libcall_names())?;
    let mut object_module = ObjectModule::new(object_builder);

    let mut func_ctx = FunctionBuilderContext::new();
    let signature = Signature {
        params: Vec::new(),
        returns: vec![AbiParam::new(I32)],
        call_conv,
    };
    let main_func_id =
        object_module.declare_function("main", Linkage::Export, &signature)?;
    ctx.func =
        Function::with_name_signature(UserFuncName::default(), signature);
    let mut fb = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);
    let block = fb.create_block();
    fb.switch_to_block(block);
    fb.seal_block(block);
    let mut compiler = Compiler {
        stack: Vec::new(),
        pointer_type,
        object_module,
        extern_functions: HashMap::new(),
        extern_function_signatures,
        strings: HashMap::new(),
    };
    compiler.compile(program, &mut fb);
    fb.finalize();

    compiler
        .object_module
        .define_function(main_func_id, &mut ctx)?;

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

struct Compiler {
    stack: Vec<Value>,
    pointer_type: Type,
    object_module: ObjectModule,
    extern_functions: HashMap<&'static str, FuncId>,
    extern_function_signatures: HashMap<&'static str, Signature>,
    strings: HashMap<&'static str, DataId>,
}

impl Stack for Compiler {
    type Item = Value;

    fn push(&mut self, element: Self::Item) {
        self.stack.push(element);
    }

    fn pop(&mut self) -> Self::Item {
        self.stack.pop().unwrap()
    }
}

impl Compiler {
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
        fb.ins().global_value(self.pointer_type, global_value)
    }

    fn compile(&mut self, program: &Program, fb: &mut FunctionBuilder) {
        for instruction in &*program.instructions {
            self.compile_instruction(instruction, fb);
        }
        let exit_code = fb.ins().iconst(I32, 0);
        fb.ins().return_(&[exit_code]);
    }

    fn compile_instruction(
        &mut self,
        instruction: &Instruction,
        fb: &mut FunctionBuilder,
    ) {
        match instruction {
            Instruction::Then(body) => self.compile_then(body, fb),
            Instruction::Push(number) => {
                self.stack.push(fb.ins().iconst(I32, i64::from(*number)));
            }
            Instruction::True => self.stack.push(fb.ins().iconst(I8, 1)),
            Instruction::False => self.stack.push(fb.ins().iconst(I8, 0)),
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

    fn compile_then(&mut self, body: &[Instruction], fb: &mut FunctionBuilder) {
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
