use crate::parser::Node;
use crate::parser::TRef;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimitiveKind {
    Void,
    Bool,
    Char,
    Short,
    Int,
    Long,
    LongLong,
    Int128,
    Float,
    Double,
    LongDouble,
}

#[derive(Debug, Clone)]
pub struct EnumVariant<'t> {
    pub name: TRef<'t>,
    pub value: Option<IntConstExpr<'t>>,
}

#[derive(Debug, Clone)]
pub enum TypeKind<'t> {
    Primitive {
        kind: PrimitiveKind,
        is_signed: bool,
        is_unsigned: bool,
    },
    StructRef {
        name: TRef<'t>,
    },
    Struct {
        name: Option<TRef<'t>>,
        members: Vec<DeclType<'t>>,
    },
    UnionRef {
        name: TRef<'t>,
    },
    Union {
        name: Option<TRef<'t>>,
        members: Vec<DeclType<'t>>,
    },
    EnumRef {
        name: TRef<'t>,
    },
    Enum {
        name: Option<TRef<'t>>,
        members: Vec<EnumVariant<'t>>,
    },
    Typedef {
        name: TRef<'t>,
    },
    Special {
        name: TRef<'t>,
    },
    Function {
        ret: Box<TypeKind<'t>>,
        args: Vec<DeclType<'t>>,
        is_variadic: bool,
    },
    Pointer {
        base: Box<TypeKind<'t>>,
        is_const: bool,
        is_volatile: bool,
    },
    Array {
        base: Box<TypeKind<'t>>,
        size: Option<IntConstExpr<'t>>,
    },
}

impl<'t> TypeKind<'t> {
    pub fn new() -> Self {
        Self::Primitive {
            kind: PrimitiveKind::Void,
            is_signed: false,
            is_unsigned: false,
        }
    }

    pub fn is_void(&self) -> bool {
        match self {
            TypeKind::Primitive {
                kind: PrimitiveKind::Void,
                ..
            } => true,
            _ => false,
        }
    }

    pub fn make_pointer(&mut self) {
        let mut base = Self::new();
        core::mem::swap(self, &mut base);
        *self = Self::Pointer {
            base: Box::new(base),
            is_const: false,
            is_volatile: false,
        };
    }

    pub fn make_array(&mut self, size: Option<IntConstExpr<'t>>) {
        let mut base = Self::new();
        core::mem::swap(self, &mut base);
        *self = Self::Array {
            base: Box::new(base),
            size,
        };
    }

    pub fn make_function(&mut self, args: Vec<DeclType<'t>>, is_variadic: bool) {
        let mut base = Self::new();
        core::mem::swap(self, &mut base);
        *self = Self::Function {
            ret: Box::new(base),
            args,
            is_variadic,
        };
    }
}

#[derive(Debug, Clone)]
pub enum IntConstExpr<'t> {
    Value(i64),
    Expression(Vec<Node<'t>>),
}

#[derive(Debug, Clone)]
pub enum Attribute<'t> {
    Packed,
    Aligned(IntConstExpr<'t>),
    Unknown(TRef<'t>),
}

#[derive(Debug, Clone)]
pub struct DeclType<'t> {
    pub token: TRef<'t>,
    pub name: Option<TRef<'t>>,
    pub attributes: Vec<Attribute<'t>>,
    pub type_kind: TypeKind<'t>,
    pub is_typedef: bool,
    pub is_typeof: bool,
    pub is_const: bool,
    pub is_volatile: bool,
    pub is_static: bool,
    pub is_inline: bool,
    pub is_noreturn: bool,
    pub is_bit_field: Option<IntConstExpr<'t>>,
    pub sizeof: Option<u64>,
    pub alignof: Option<u64>,
}

impl<'t> DeclType<'t> {
    pub fn new(token: TRef<'t>) -> Self {
        Self {
            token,
            name: None,
            attributes: Vec::new(),
            type_kind: TypeKind::new(),
            is_typedef: false,
            is_typeof: false,
            is_const: false,
            is_static: false,
            is_volatile: false,
            is_inline: false,
            is_noreturn: false,
            is_bit_field: None,
            sizeof: None,
            alignof: None,
        }
    }
}
