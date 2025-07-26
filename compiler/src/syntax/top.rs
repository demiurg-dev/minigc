use std::collections::BTreeMap;

use crate::syntax::{Expr, Fnc, StructType, Type};

#[derive(Debug, Clone, Default)]
pub struct Top {
    pub structs: BTreeMap<String, StructType>,
    pub fncs: BTreeMap<String, Fnc>,
}

pub(super) type Ctx<'a> = im::HashMap<&'a String, (&'a Type, bool)>;

impl Top {
    pub fn check(&self) -> Result<(), CheckError> {
        for fnc in self.fncs.values() {
            // TODO: Check for invalid/duplicate names

            let mut ctx: Ctx = Default::default();
            for param in fnc.ty.params.iter() {
                ctx.insert(&param.name, (&param.ty, false));
            }
            fnc.body.check(&ctx, &self.fncs, &fnc.ty.ret)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum CheckError {
    #[error("expected integer type, found {actual:?} in expression {expr:?}")]
    ExpectedIntegerType { actual: Type, expr: Expr },
    #[error("expected type {expected:?}, found {actual:?} in expression {expr:?}")]
    TypeMismatch { actual: Type, expected: Type, expr: Expr },
    #[error("standalone let expression")]
    StandaloneLet { expr: Expr },
    #[error("unknown variable name: '{0}'")]
    UnknownVariable(String),
    #[error("unknown function name: '{0}'")]
    UnknownFunction(String),
    #[error("invalid number of argument in call to '{name}', expected {expected}, found {found}")]
    InvalidArgNum { name: String, expected: usize, found: usize },
    #[error("assignment to immutable variable '{0}'")]
    AssignmentToImmutable(String),
}
