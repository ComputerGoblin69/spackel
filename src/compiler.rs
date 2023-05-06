use crate::ir::{Instruction, Program};
use anyhow::{Context as _, Result};
use cranelift::{
    codegen::{
        ir::{Function, Inst, UserFuncName},
        Context,
    },
    prelude::{
        isa::TargetIsa, settings, types::I32, AbiParam, Configurable,
        FunctionBuilder, FunctionBuilderContext, InstBuilder, Signature, Type,
        Value,
    },
};
use cranelift_module::{DataContext, DataId, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use std::{collections::HashMap, fs::File, io::Write, path::Path};

pub struct Options<'a> {
    pub target_triple: &'a str,
    pub out_path: &'a Path,
}

pub fn compile(program: &Program, options: &Options) -> Result<()> {
    let mut shared_builder = settings::builder();
    shared_builder.enable("is_pic")?;

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
    compiler.compile(program, &mut fb)?;
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

impl Compiler {
    fn pop(&mut self) -> Result<Value> {
        self.stack.pop().context("not enough arguments on stack")
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

    fn compile(
        &mut self,
        program: &Program,
        fb: &mut FunctionBuilder,
    ) -> Result<()> {
        for instruction in &program.instructions {
            self.compile_instruction(instruction, fb)?;
        }
        let exit_code = fb.ins().iconst(I32, 0);
        fb.ins().return_(&[exit_code]);
        Ok(())
    }

    fn compile_instruction(
        &mut self,
        instruction: &Instruction,
        fb: &mut FunctionBuilder,
    ) -> Result<()> {
        match *instruction {
            Instruction::Push(number) => {
                self.stack.push(fb.ins().iconst(I32, i64::from(number)));
            }
            Instruction::Println => {
                let n = self.pop()?;
                let fmt = self.allocate_str("%d\n\0", fb);
                self.call_extern("printf", &[fmt, n], fb);
            }
            Instruction::PrintChar => {
                let n = self.pop()?;
                self.call_extern("spkl_print_char", &[n], fb);
            }
            Instruction::Add => {
                // FIXME: This does not implement the interpreter's quirks.
                let b = self.pop()?;
                let a = self.pop()?;
                self.stack.push(fb.ins().iadd(a, b));
            }
            Instruction::Sub => {
                let b = self.pop()?;
                let a = self.pop()?;
                self.stack.push(fb.ins().isub(a, b));
            }
            Instruction::Mul => {
                let b = self.pop()?;
                let a = self.pop()?;
                self.stack.push(fb.ins().imul(a, b));
            }
            Instruction::Div => {
                let b = self.pop()?;
                let a = self.pop()?;
                self.stack.push(fb.ins().sdiv(a, b));
            }
            Instruction::Rem => {
                let b = self.pop()?;
                let a = self.pop()?;
                self.stack.push(fb.ins().srem(a, b));
            }
            Instruction::Pop => {
                self.pop()?;
            }
            Instruction::Dup => {
                let v = self.pop()?;
                self.stack.push(v);
                self.stack.push(v);
            }
            Instruction::Swap => {
                let b = self.pop()?;
                let a = self.pop()?;
                self.stack.push(b);
                self.stack.push(a);
            }
            Instruction::Over => {
                let b = self.pop()?;
                let a = self.pop()?;
                self.stack.push(a);
                self.stack.push(b);
                self.stack.push(a);
            }
        }
        Ok(())
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
