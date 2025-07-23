use crate::syntax::{CheckError, Ctx, Type};

#[derive(Debug, Clone)]
pub enum Expr {
    Const(i64),
    Var(String),
    BinOp { op: BinOp, left: Box<Expr>, right: Box<Expr> },
    Let { name: String, ty: Type, rhs: Box<Expr> },
    Ref(Box<Expr>),
    Block(Box<[Expr]>),
    Unit,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
}

macro_rules! expected_type {
    ($expected:expr, $actual:expr, $expr:expr) => {
        Err(crate::syntax::CheckError::TypeMismatch {
            expected: $expected.clone(),
            actual: $actual.clone(),
            expr: $expr.clone(),
        })
    };
}

impl Expr {
    pub(super) fn check<'a>(&'a self, ctx: &Ctx<'a>, ty: &Type) -> Result<(), CheckError> {
        match self {
            Self::Const(_) => self.ensure_integer_type(ty),
            Self::Var(name) => match ctx.get(name) {
                None => Err(CheckError::UnknownName(name.to_string())),
                Some(ty2) => {
                    if ty == *ty2 {
                        Ok(())
                    } else {
                        expected_type!(ty, *ty2, self)
                    }
                }
            },
            Self::BinOp { op, left, right } => match op {
                BinOp::Add => {
                    self.ensure_integer_type(ty)?;
                    left.check(ctx, ty)?;
                    right.check(ctx, ty)
                }
            },
            Self::Let { .. } => Err(CheckError::StandaloneLet { expr: self.clone() }),
            Self::Ref(expr) => {
                let ty = self.ensure_ref_type(ty)?;
                expr.check(ctx, ty)
            }
            Self::Block(stmts) => {
                let mut ctx: Ctx<'a> = ctx.clone();
                if stmts.is_empty() {
                    return match ty {
                        Type::Unit => Ok(()),
                        _ => expected_type!(ty, Type::Unit, self.clone()),
                    };
                }

                let last_id = stmts.len() - 1;
                for (i, stmt) in stmts.iter().enumerate() {
                    match stmt {
                        Expr::Let { name, ty, rhs } if i < last_id => {
                            rhs.check(&ctx, ty)?;
                            ctx.insert(name, ty);
                        }
                        _ if i < last_id => stmt.check(&ctx, &Type::Unit)?,
                        _ => {
                            stmt.check(&ctx, ty)?;
                        }
                    }
                }

                Ok(())
            }
            Self::Unit => match ty {
                Type::Unit => Ok(()),
                _ => expected_type!(ty.clone(), Type::Unit, self.clone()),
            },
        }
    }

    fn ensure_integer_type(&self, ty: &Type) -> Result<(), CheckError> {
        match ty {
            Type::Int { .. } => Ok(()),
            _ => Err(CheckError::ExpectedIntegerType { actual: ty.clone(), expr: self.clone() }),
        }
    }

    fn ensure_ref_type<'a>(&self, ty: &'a Type) -> Result<&'a Type, CheckError> {
        match ty {
            Type::Ref(ty) => Ok(ty),
            _ => Err(CheckError::ExpectedRefType { actual: ty.clone(), expr: self.clone() }),
        }
    }
}
