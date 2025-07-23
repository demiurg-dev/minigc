#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int { size: IntSize, signed: bool },
    Tuple(Box<[Type]>),
    Name(String),
    Ref(Box<Type>),
    Unit,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntSize {
    I8,
    I16,
    I32,
    I64,
}

#[derive(Debug, Clone)]
pub struct FncType {
    pub params: Box<[FncParam]>,
    pub ret: Type,
}

#[derive(Debug, Clone)]
pub struct FncParam {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct StructType {
    pub fields: Box<[StructField]>,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: String,
    pub ty: Type,
}
