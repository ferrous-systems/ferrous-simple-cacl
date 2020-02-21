use std::fmt;

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

impl fmt::Display for CalcError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CalcError::Eof => write!(f, "eof"),
            CalcError::BadToken => write!(f, "bad token"),
            CalcError::ArithError => write!(f, "arithmetic error"),
        }
    }
}

impl std::error::Error for CalcError {}

/// Parse input into an `Expression`.
pub fn parse(input: &str) -> Result<Expression, CalcError> {
    let tokens = input.split_ascii_whitespace().collect::<Vec<&str>>();
    let first = match tokens.get(0) {
        Some(it) => *it,
        None => return Err(CalcError::Eof),
    };

    let bin_op = match first {
        "+" => BinOp::Add,
        "-" => BinOp::Sub,
        "*" => BinOp::Mul,
        "/" => BinOp::Div,
        "sqr" => {
            let arg = match tokens.get(1) {
                None => return Err(CalcError::Eof),
                Some(token) => match token.parse::<i64>() {
                    Ok(n) => n,
                    Err(_) => return Err(CalcError::BadToken),
                },
            };
            return Ok(Expression::Sqr(arg));
        }
        _ => {
            return match first.parse::<i64>() {
                Ok(n) => Ok(Expression::Number(n)),
                Err(_) => Err(CalcError::BadToken),
            }
        }
    };
    let lhs = match tokens.get(1) {
        None => return Err(CalcError::Eof),
        Some(token) => match token.parse::<i64>() {
            Ok(n) => n,
            Err(_) => return Err(CalcError::BadToken),
        },
    };
    let rhs = match tokens.get(2) {
        None => return Err(CalcError::Eof),
        Some(token) => match token.parse::<i64>() {
            Ok(n) => n,
            Err(_) => return Err(CalcError::BadToken),
        },
    };

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
