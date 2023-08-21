use crate::{lexer::Token, typ::Type};
use codemap::Span;
use std::collections::HashMap;

pub struct Program<'src> {
    pub functions: HashMap<&'src str, Function>,
}

pub struct Function {
    pub declaration_span: Span,
    pub parameters: Box<Block>,
    pub returns: Box<Block>,
    pub body: Box<Block>,
    pub end_span: Span,
}

pub type Block<T = Span> = [(Instruction<T>, T)];

#[derive(Clone, Debug)]
pub enum Instruction<T = Span> {
    Call(Box<str>),
    Then(Box<Block<T>>),
    ThenElse(Box<Block<T>>, Box<Block<T>>),
    Repeat { body: Box<Block<T>>, end_span: Span },
    Unsafe(Box<Block<T>>),
    PushI32(i32),
    PushF32(f32),
    PushBool(bool),
    PushType(Type),
    Ptr,
    TypeOf,
    Print,
    Println,
    PrintChar,
    BinMathOp(BinMathOp),
    Sqrt,
    Comparison(Comparison),
    Not,
    BinLogicOp(BinLogicOp),
    AddrOf,
    ReadPtr,
    Drop,
    Dup,
    Swap,
    Over,
    Nip,
    Tuck,
}

impl From<Token<'_>> for Instruction {
    fn from(token: Token) -> Self {
        match &*token {
            "true" => Self::PushBool(true),
            "false" => Self::PushBool(false),
            "i32" => Self::PushType(Type::I32),
            "bool" => Self::PushType(Type::Bool),
            "type" => Self::PushType(Type::Type),
            "ptr" => Self::Ptr,
            "type-of" => Self::TypeOf,
            "print" => Self::Print,
            "println" => Self::Println,
            "print-char" => Self::PrintChar,
            "+" => Self::BinMathOp(BinMathOp::Add),
            "-" => Self::BinMathOp(BinMathOp::Sub),
            "*" => Self::BinMathOp(BinMathOp::Mul),
            "/" => Self::BinMathOp(BinMathOp::Div),
            "%" => Self::BinMathOp(BinMathOp::Rem),
            "+🤡" => Self::BinMathOp(BinMathOp::SillyAdd),
            "sqrt" => Self::Sqrt,
            "<" => Self::Comparison(Comparison::Lt),
            "<=" => Self::Comparison(Comparison::Le),
            "=" => Self::Comparison(Comparison::Eq),
            ">" => Self::Comparison(Comparison::Ge),
            ">=" => Self::Comparison(Comparison::Gt),
            "not" => Self::Not,
            "and" => Self::BinLogicOp(BinLogicOp::And),
            "or" => Self::BinLogicOp(BinLogicOp::Or),
            "xor" => Self::BinLogicOp(BinLogicOp::Xor),
            "nand" => Self::BinLogicOp(BinLogicOp::Nand),
            "nor" => Self::BinLogicOp(BinLogicOp::Nor),
            "xnor" => Self::BinLogicOp(BinLogicOp::Xnor),
            "addr-of" => Self::AddrOf,
            "read-ptr" => Self::ReadPtr,
            "ß" => Self::PushI32(1945),
            "drop" => Self::Drop,
            "dup" => Self::Dup,
            "swap" => Self::Swap,
            "over" => Self::Over,
            "nip" => Self::Nip,
            "tuck" => Self::Tuck,
            _ =>
            {
                #[allow(clippy::option_if_let_else)]
                if let Ok(number) = token.parse::<i32>() {
                    Self::PushI32(number)
                } else if let Ok(number) = token.parse::<f32>() {
                    Self::PushF32(number)
                } else {
                    Self::Call(token.text.into())
                }
            }
        }
    }
}

impl<T> Instruction<T> {
    pub const fn is_unsafe(&self) -> bool {
        matches!(self, Self::ReadPtr)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum BinMathOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    SillyAdd,
}

#[derive(Clone, Copy, Debug)]
pub enum Comparison {
    Lt,
    Le,
    Eq,
    Ge,
    Gt,
}

#[derive(Clone, Copy, Debug)]
pub enum BinLogicOp {
    And,
    Or,
    Xor,
    Nand,
    Nor,
    Xnor,
}
