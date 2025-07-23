use crate::syntax::{Expr, FncType};

#[derive(Debug, Clone)]
pub struct Fnc {
    pub ty: FncType,
    pub body: Expr,
}
