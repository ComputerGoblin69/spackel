use crate::ir::Program;
use anyhow::Result;
use cranelift::{
    codegen::{
        ir::{Function, UserFuncName},
        Context,
    },
    prelude::{
        settings, types::I32, AbiParam, FunctionBuilder,
        FunctionBuilderContext, InstBuilder, Signature,
    },
};
use cranelift_module::{Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use std::{fs::File, io::Write, path::Path};

pub struct CompilationOptions<'a> {
    pub target_triple: &'a str,
    pub out_path: &'a Path,
}

pub fn compile(program: &Program, options: &CompilationOptions) -> Result<()> {
    let shared_builder = settings::builder();
    let shared_flags = settings::Flags::new(shared_builder);
    let isa = cranelift::codegen::isa::lookup_by_name(options.target_triple)?
        .finish(shared_flags)?;
    let call_conv = isa.default_call_conv();

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
    let exit_code = fb.ins().iconst(I32, 0);
    fb.ins().return_(&[exit_code]);
    fb.finalize();

    object_module.define_function(main_func_id, &mut ctx)?;

    let object_bytes = object_module.finish().emit()?;
    let mut object_file = File::create(options.out_path)?;
    object_file.write_all(&object_bytes)?;

    Ok(())
}
