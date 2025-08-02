use std::collections::BTreeMap;

use hashbrown::HashMap;

use crate::syntax::{CheckError, Ctx, Fnc, IntSize, StructField, StructType, Type};

#[derive(Debug, Clone)]
pub enum Expr {
    Const(i64),
    Var(String),
    StructField { base: String, name: String },
    Struct { name: String, exprs: Box<[(String, Expr)]> },
    BinOp { op: BinOp, left: Box<Expr>, right: Box<Expr> },
    UnOp { op: UnOp, expr: Box<Expr> },
    Ite { cond: Box<Expr>, then_branch: Box<Expr>, else_branch: Option<Box<Expr>> },
    Let { name: String, ty: Type, is_mut: bool, rhs: Box<Expr> },
    Assign { name: String, expr: Box<Expr> },
    Call { name: String, args: Box<[Expr]> },
    While { cond: Box<Expr>, body: Box<Expr> },
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
        structs: &BTreeMap<String, StructType>,
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
            Self::StructField { base, name } => match ctx.get(base) {
                Some((s_ty, _)) => match s_ty {
                    Type::Name(s_name) => {
                        let strct = structs.get(s_name).unwrap();
                        match strct.fields.iter().find(|f| &f.name == name) {
                            Some(fld) => {
                                if *ty == fld.ty {
                                    Ok(())
                                } else {
                                    expected_type!(ty, fld.ty, self)
                                }
                            }
                            None => Err(CheckError::InvalidField { st: name.to_string(), name: s_name.to_string() }),
                        }
                    }
                    _ => Err(CheckError::ExpectedStructType { actual: (*s_ty).clone(), expr: self.clone() }),
                },
                None => Err(CheckError::UnknownVariable(base.to_string())),
            },
            Self::Struct { name, exprs } => {
                match ty {
                    Type::Name(s_name) if name == s_name => {
                        let strct = structs.get(s_name).unwrap();
                        // TODO: We could have duplicate fields, so need to handle that as well
                        let mut available_fields: HashMap<&String, &Expr> =
                            HashMap::from_iter(exprs.iter().map(|(n, e)| (n, e)));
                        for StructField { name, ty } in &strct.fields {
                            match available_fields.remove(name) {
                                Some(e) => e.check(ctx, fncs, structs, ty)?,
                                None => {
                                    return Err(CheckError::MissingField {
                                        st: s_name.to_string(),
                                        name: name.to_string(),
                                    });
                                }
                            }
                        }
                        match available_fields.iter().next() {
                            Some((name, _expr)) => {
                                Err(CheckError::InvalidField { st: s_name.to_string(), name: name.to_string() })
                            }
                            None => Ok(()),
                        }
                    }
                    Type::Name(_) => {
                        expected_type!(*ty, &Type::Name(name.to_string()), self)
                    }
                    _ => Err(CheckError::ExpectedStructType { actual: (*ty).clone(), expr: self.clone() }),
                }
            }
            Self::BinOp { op, left, right } => match op {
                BinOp::Add | BinOp::Sub | BinOp::Mul => {
                    self.ensure_integer_type(ty)?;
                    left.check(ctx, fncs, structs, ty)?;
                    right.check(ctx, fncs, structs, ty)
                }
                BinOp::Leq => {
                    if !matches!(ty, Type::Bool) {
                        return expected_type!(ty, Type::Bool, self);
                    }
                    // TODO: We cannot check proper type here (need a bit of relaxation)
                    left.check(ctx, fncs, structs, &Type::Int { size: IntSize::I64, signed: true })?;
                    right.check(ctx, fncs, structs, &Type::Int { size: IntSize::I64, signed: true })
                }
            },
            Self::UnOp { op, expr } => match op {
                UnOp::Neg => {
                    self.ensure_integer_type(ty)?;
                    expr.check(ctx, fncs, structs, ty)
                }
            },
            Self::Ite { cond, then_branch, else_branch } => {
                cond.check(ctx, fncs, structs, &Type::Bool)?;
                let ty = match else_branch {
                    Some(else_branch) => {
                        else_branch.check(ctx, fncs, structs, ty)?;
                        ty
                    }
                    None => &Type::Unit,
                };
                then_branch.check(ctx, fncs, structs, ty)
            }
            Self::Let { .. } => Err(CheckError::StandaloneLet { expr: self.clone() }),
            Self::Assign { name, expr } => {
                if !matches!(ty, Type::Unit) {
                    return expected_type!(Type::Unit, ty, self);
                }
                match ctx.get(name) {
                    Some((ty, is_mut)) => {
                        if *is_mut {
                            expr.check(ctx, fncs, structs, ty)
                        } else {
                            Err(CheckError::AssignmentToImmutable(name.to_string()))
                        }
                    }
                    None => Err(CheckError::UnknownVariable(name.to_string())),
                }
            }
            Self::Call { name, args } => {
                // Note: ugly hack to avoid typechecking internals
                if name.starts_with("__") {
                    return Ok(());
                }

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
                    expr.check(ctx, fncs, structs, &param.ty)?;
                }
                Ok(())
            }
            Self::While { cond, body } => {
                if !matches!(ty, Type::Unit) {
                    return expected_type!(ty, Type::Unit, self);
                }
                cond.check(ctx, fncs, structs, &Type::Bool)?;
                body.check(ctx, fncs, structs, ty)
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
                            rhs.check(&ctx, fncs, structs, ty)?;
                            ctx.insert(name, (ty, *is_mut));
                        }
                        _ if i < last_id => stmt.check(&ctx, fncs, structs, &Type::Unit)?,
                        _ => {
                            stmt.check(&ctx, fncs, structs, ty)?;
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
}
