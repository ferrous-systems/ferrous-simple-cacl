// extern crate core;
// use core::prelude::*;

// #![no_std]

use core::fmt;

use std::io;

#[derive(PartialEq, Eq, Debug)]
pub enum Expression {
    /// 92
    Number(i64),
    /// sqr 42
    Sqr(i64),
    /// + 3 4
    BinExpression { bin_op: BinOp, lhs: i64, rhs: i64 },
}

#[derive(PartialEq, Eq, Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(PartialEq, Eq, Debug)]
pub enum CalcError {
    Eof,
    BadToken,
    ArithError,
}

impl std::error::Error for CalcError {}

impl From<ParseIntError> for CalcError {
    fn from(_: ParseIntError) -> Self {
        unimplemented!()
    }
}

impl fmt::Display for CalcError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CalcError::Eof => write!(f, "eof"),
            CalcError::BadToken => write!(f, "bad token"),
            CalcError::ArithError => write!(f, "arithmetic error"),
        }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for CalcError {}

fn failable() -> Result<i64, AError> {
    Ok(92)
}

fn modify(x: i64) -> Result<i64, BError> {
    Ok(x * 2)
}

fn foo() -> Result<i64, MetaError> {
    let x = failable()?;
    let res = modify(x)?;
    Ok((x + 1))
}

/// Parse input into an `Expression`.
pub fn parse(input: &str) -> Result<Expression, Box<dyn std::error::Error>> {
    let mut tokens = input.split_ascii_whitespace();
    let first = tokens.next().ok_or(CalcError::Eof)?;

    let bin_op = match first {
        "+" => BinOp::Add,
        "-" => BinOp::Sub,
        "*" => BinOp::Mul,
        "/" => BinOp::Div,
        "sqr" => {
            let arg = tokens.next().ok_or(CalcError::Eof)?;
            let arg = arg.parse::<i64>()?;
            return Ok(Expression::Sqr(arg));
        }
        _ => {
            let arg = tokens.next().ok_or(CalcError::Eof)?;
            let arg = arg.parse::<i64>()?;
            return Ok(Expression::Number(arg));
        }
    };
    let lhs = tokens.next().ok_or(CalcError::Eof)?;
    let lhs = lhs.parse::<i64>()?;

    let rhs = tokens.next().ok_or(CalcError::Eof)?;
    let rhs = rhs.parse::<i64>()?;

    Ok(Expression::BinExpression { bin_op, lhs, rhs })
}

/// Evaluates an expression.
pub fn eval(expr: Expression) -> Result<i64, CalcError> {
    let res = match expr {
        Expression::Number(n) => n,
        Expression::Sqr(n) => n * n,
        Expression::BinExpression { bin_op, lhs, rhs } => {
            let op_res = match bin_op {
                BinOp::Add => lhs.checked_add(rhs),
                BinOp::Sub => lhs.checked_sub(rhs),
                BinOp::Mul => lhs.checked_mul(rhs),
                BinOp::Div => lhs.checked_div(rhs),
            };

            match op_res {
                Some(it) => it,
                None => return Err(CalcError::ArithError),
            }
        }
    };
    Ok(res)
}

#[test]
fn test() {
    assert_eq!(parse("92"), Ok(Expression::Number(92)),);
    assert_eq!(parse("sqr 92"), Ok(Expression::Sqr(92)),);
    assert_eq!(
        parse("+ 90 2"),
        Ok(Expression::BinExpression {
            bin_op: BinOp::Add,
            lhs: 90,
            rhs: 2,
        }),
    );
}

use core::num::ParseIntError;
use core::panic::PanicInfo;

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {
        continue;
    }
}
