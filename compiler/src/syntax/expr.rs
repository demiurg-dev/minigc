use std::collections::BTreeMap;

use crate::syntax::{CheckError, Ctx, Fnc, IntSize, Type};

#[derive(Debug, Clone)]
pub enum Expr {
    Const(i64),
    Var(String),
    BinOp { op: BinOp, left: Box<Expr>, right: Box<Expr> },
    UnOp { op: UnOp, expr: Box<Expr> },
    Ite { cond: Box<Expr>, then_branch: Box<Expr>, else_branch: Option<Box<Expr>> },
    Let { name: String, ty: Type, is_mut: bool, rhs: Box<Expr> },
    Assign { name: String, expr: Box<Expr> },
    Call { name: String, args: Box<[Expr]> },
    Ref(Box<Expr>),
    Block(Box<[Expr]>),
    Unit,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Leq,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    Neg,
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
    pub(super) fn check<'a>(
        &'a self,
        ctx: &Ctx<'a>,
        fncs: &BTreeMap<String, Fnc>,
        ty: &Type,
    ) -> Result<(), CheckError> {
        match self {
            Self::Const(_) => self.ensure_integer_type(ty),
            Self::Var(name) => match ctx.get(name) {
                None => Err(CheckError::UnknownVariable(name.to_string())),
                Some((ty2, ..)) => {
                    if ty == *ty2 {
                        Ok(())
                    } else {
                        expected_type!(ty, *ty2, self)
                    }
                }
            },
            Self::BinOp { op, left, right } => match op {
                BinOp::Add | BinOp::Sub | BinOp::Mul => {
                    self.ensure_integer_type(ty)?;
                    left.check(ctx, fncs, ty)?;
                    right.check(ctx, fncs, ty)
                }
                BinOp::Leq => {
                    if !matches!(ty, Type::Bool) {
                        return expected_type!(ty, Type::Bool, self);
                    }
                    // TODO: We cannot check proper type here (need a bit of relaxation)
                    left.check(ctx, fncs, &Type::Int { size: IntSize::I64, signed: true })?;
                    right.check(ctx, fncs, &Type::Int { size: IntSize::I64, signed: true })
                }
            },
            Self::UnOp { op, expr } => match op {
                UnOp::Neg => {
                    self.ensure_integer_type(ty)?;
                    expr.check(ctx, fncs, ty)
                }
            },
            Self::Ite { cond, then_branch, else_branch } => {
                cond.check(ctx, fncs, &Type::Bool)?;
                let ty = match else_branch {
                    Some(else_branch) => {
                        else_branch.check(ctx, fncs, ty)?;
                        ty
                    }
                    None => &Type::Unit,
                };
                then_branch.check(ctx, fncs, ty)
            }
            Self::Let { .. } => Err(CheckError::StandaloneLet { expr: self.clone() }),
            Self::Assign { name, expr } => {
                if !matches!(ty, Type::Unit) {
                    return expected_type!(Type::Unit, ty, self);
                }
                match ctx.get(name) {
                    Some((ty, is_mut)) => {
                        if *is_mut {
                            expr.check(ctx, fncs, ty)
                        } else {
                            Err(CheckError::AssignmentToImmutable(name.to_string()))
                        }
                    }
                    None => Err(CheckError::UnknownVariable(name.to_string())),
                }
            }
            Self::Call { name, args } => {
                let fnc = fncs
                    .get(name)
                    .ok_or_else(|| CheckError::UnknownFunction(name.clone()))?;
                if fnc.ty.params.len() != args.len() {
                    return Err(CheckError::InvalidArgNum {
                        name: name.clone(),
                        expected: fnc.ty.params.len(),
                        found: args.len(),
                    });
                }
                if fnc.ty.ret != *ty {
                    return expected_type!(ty, &fnc.ty.ret, self);
                }
                for (param, expr) in fnc.ty.params.iter().zip(args) {
                    expr.check(ctx, fncs, &param.ty)?;
                }
                Ok(())
            }
            Self::Ref(expr) => {
                let ty = self.ensure_ref_type(ty)?;
                expr.check(ctx, fncs, ty)
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
                        Expr::Let { name, ty, is_mut, rhs } if i < last_id => {
                            rhs.check(&ctx, fncs, ty)?;
                            ctx.insert(name, (ty, *is_mut));
                        }
                        _ if i < last_id => stmt.check(&ctx, fncs, &Type::Unit)?,
                        _ => {
                            stmt.check(&ctx, fncs, ty)?;
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
