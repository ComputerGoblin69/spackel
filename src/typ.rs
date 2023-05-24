use crate::{
    diagnostics::{self, primary_label},
    ir::{Function, Instruction, Program},
};
use anyhow::{ensure, Context, Result};
use codemap::{Span, Spanned};
use itertools::Itertools;
use std::{collections::HashMap, fmt};

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Bool,
    I32,
    F32,
    Type,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Bool => "bool",
            Self::I32 => "i32",
            Self::F32 => "f32",
            Self::Type => "type",
        })
    }
}

pub struct CheckedProgram {
    functions: HashMap<String, CheckedFunction>,
}

impl CheckedProgram {
    pub const fn functions(&self) -> &HashMap<String, CheckedFunction> {
        &self.functions
    }
}

pub struct CheckedFunction {
    declaration_span: Span,
    pub signature: FunctionSignature,
    pub body: Box<[Spanned<Instruction>]>,
}

#[derive(Clone)]
pub struct FunctionSignature {
    pub parameters: Box<[Type]>,
    pub returns: Box<[Type]>,
}

pub fn check(program: Program) -> Result<CheckedProgram> {
    let function_signatures = program
        .functions
        .iter()
        .map(|(name, function)| {
            let parameters = function
                .parameters
                .iter()
                .map(|param| match &**param {
                    Instruction::PushType(typ) => Ok(*typ),
                    _ => Err(diagnostics::error(
                        "unsupported instruction in function signature"
                            .to_owned(),
                        vec![primary_label(param.span, "")],
                    )),
                })
                .collect::<Result<Box<_>, _>>()?;
            let returns = function
                .returns
                .iter()
                .map(|param| match &**param {
                    Instruction::PushType(typ) => Ok(*typ),
                    _ => Err(diagnostics::error(
                        "unsupported instruction in function signature"
                            .to_owned(),
                        vec![primary_label(param.span, "")],
                    )),
                })
                .collect::<Result<Box<_>, _>>()?;
            Ok((
                name.clone(),
                FunctionSignature {
                    parameters,
                    returns,
                },
            ))
        })
        .collect::<Result<_>>()?;

    Checker {
        stack: Vec::new(),
        function_signatures,
    }
    .check(program)
}

struct Checker {
    stack: Vec<Type>,
    function_signatures: HashMap<String, FunctionSignature>,
}

impl Checker {
    fn check(&mut self, program: Program) -> Result<CheckedProgram> {
        let checked_program = CheckedProgram {
            functions: program
                .functions
                .into_iter()
                .map(|(name, function)| {
                    let checked_function =
                        self.check_function(&name, function)?;
                    Ok((name, checked_function))
                })
                .collect::<Result<_>>()?,
        };

        let main = checked_program
            .functions
            .get("main")
            .context("program has no `main` function")?;
        ensure!(
            main.signature.parameters.is_empty()
                && main.signature.returns.is_empty(),
            diagnostics::error(
                "`main` function has wrong signature".to_owned(),
                vec![primary_label(main.declaration_span, "defined here")]
            )
            .note("`main` must have no parameters and no return values")
        );

        Ok(checked_program)
    }

    fn check_function(
        &mut self,
        name: &str,
        function: Function,
    ) -> Result<CheckedFunction> {
        self.stack = self.function_signatures[name].parameters.to_vec();
        for instruction in &*function.body {
            self.check_instruction(instruction)?;
        }

        self.transform(
            &self.function_signatures[name]
                .returns
                .iter()
                .copied()
                .map(Parameter::Concrete)
                .collect::<Box<_>>(),
            &[],
            function.end_span,
        )?;
        ensure!(
            self.stack.is_empty(),
            diagnostics::error(
                format!(
                    "there are values left on the stack with the following types: `{}`",
                    self.stack.iter().format(" ")
                ),
                vec![primary_label(function.end_span, "")]
            )
        );

        Ok(CheckedFunction {
            declaration_span: function.declaration_span,
            signature: self.function_signatures[name].clone(),
            body: function.body,
        })
    }

    fn transform(
        &mut self,
        inputs: &[Parameter],
        outputs: &[Return],
        span: Span,
    ) -> Result<()> {
        ensure!(
            (self.stack.len() >= inputs.len()
                && self.stack[self.stack.len() - inputs.len()..] == *inputs),
            diagnostics::error(
                "type mismatch".to_owned(),
                vec![primary_label(
                    span,
                    format!(
                        "expected types `{}` but got `{}`",
                        inputs.iter().format(" "),
                        self.stack.iter().format(" ")
                    )
                )],
            ),
        );
        let new_len = self.stack.len() - inputs.len();
        let consumed = self.stack.split_off(new_len);

        self.stack.extend(outputs.iter().map(|&out| match out {
            Return::Concrete(typ) => typ,
            Return::Get(i) => consumed[i],
        }));
        Ok(())
    }

    fn check_instruction(
        &mut self,
        instruction: &Spanned<Instruction>,
    ) -> Result<()> {
        use Parameter::{Any, Concrete as P};
        use Return::{Concrete as R, Get};
        use Type::{Bool, F32, I32};

        let parameters;
        let returns;
        let (inputs, outputs): (&[Parameter], &[Return]) = match &**instruction
        {
            Instruction::Call(name) => {
                ensure!(
                    **name != *"main",
                    diagnostics::error(
                        "`main` cannot be called".to_owned(),
                        vec![primary_label(instruction.span, "")]
                    ).note("`main` implicitly returns the program exit code, making its signature not match up with what the source code indicates")
                );

                let signature =
                    self.function_signatures.get(&**name).ok_or_else(|| {
                        diagnostics::error(
                            format!("unknown instruction: `{name}`"),
                            vec![primary_label(instruction.span, "")],
                        )
                    })?;
                parameters = signature
                    .parameters
                    .iter()
                    .copied()
                    .map(P)
                    .collect::<Box<_>>();
                returns = signature
                    .returns
                    .iter()
                    .copied()
                    .map(R)
                    .collect::<Box<_>>();
                (&*parameters, &*returns)
            }
            Instruction::Then(_) | Instruction::ThenElse(..) => {
                (&[P(Bool)], &[])
            }
            Instruction::PushI32(_) => (&[], &[R(I32)]),
            Instruction::PushF32(_) => (&[], &[R(F32)]),
            Instruction::PushBool(_) => (&[], &[R(Bool)]),
            Instruction::PushType(_) => (&[], &[R(Type::Type)]),
            Instruction::TypeOf => (&[Any], &[R(Type::Type)]),
            Instruction::BinMathOp(_) => (&[P(I32); 2], &[R(I32)]),
            Instruction::F32BinMathOp(_) => (&[P(F32); 2], &[R(F32)]),
            Instruction::Comparison(_) => (&[P(I32); 2], &[R(Bool)]),
            Instruction::Print
            | Instruction::Println
            | Instruction::PrintChar => (&[P(I32)], &[]),
            Instruction::Not => (&[P(Bool)], &[R(Bool)]),
            Instruction::BinLogicOp(_) => (&[P(Bool); 2], &[R(Bool)]),
            Instruction::Drop => (&[Any], &[]),
            Instruction::Dup => (&[Any], &[Get(0); 2]),
            Instruction::Swap => (&[Any; 2], &[Get(1), Get(0)]),
            Instruction::Over => (&[Any; 2], &[Get(0), Get(1), Get(0)]),
            Instruction::Nip => (&[Any; 2], &[Get(1)]),
            Instruction::Tuck => (&[Any; 2], &[Get(1), Get(0), Get(1)]),
        };
        self.transform(inputs, outputs, instruction.span)?;

        match &**instruction {
            Instruction::Then(body) => {
                let before = self.stack.clone();
                for instruction in &**body {
                    self.check_instruction(instruction)?;
                }
                ensure!(
                    before == self.stack,
                    diagnostics::error(
                        format!(
                            "`then` statement changes types from `{}` to `{}`",
                            before.iter().format(" "),
                            self.stack.iter().format(" "),
                        ),
                        vec![primary_label(instruction.span, "")],
                    ),
                );
            }
            Instruction::ThenElse(then, else_) => {
                let before = self.stack.clone();
                for instruction in &**then {
                    self.check_instruction(instruction)?;
                }
                let then_types = std::mem::replace(&mut self.stack, before);
                for instruction in &**else_ {
                    self.check_instruction(instruction)?;
                }
                ensure!(
                    then_types == self.stack,
                    diagnostics::error(
                        format!(
                            "`then else` statement diverges between types `{}` and `{}`",
                            then_types.iter().format(" "),
                            self.stack.iter().format(" "),
                        ),
                        vec![primary_label(instruction.span, "")],
                    ),
                );
            }
            _ => {}
        }

        Ok(())
    }
}

#[derive(Clone, Copy)]
enum Parameter {
    Concrete(Type),
    Any,
}

impl PartialEq<Parameter> for Type {
    fn eq(&self, other: &Parameter) -> bool {
        match other {
            Parameter::Concrete(typ) => self == typ,
            Parameter::Any => true,
        }
    }
}

impl fmt::Display for Parameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Concrete(typ) => typ.fmt(f),
            Self::Any => f.write_str("<any>"),
        }
    }
}

#[derive(Clone, Copy)]
enum Return {
    Concrete(Type),
    Get(usize),
}
