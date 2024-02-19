use crate::tokenizer::{TKind, Token};
use crate::utils::{ByteStr, FastHashSet};

#[derive(Debug)]
pub struct ErrorData {
    pub message: Box<str>,
    pub token: Box<str>,
    pub file_name: Box<str>,
    pub line_number: u32,
}

#[derive(Debug)]
pub struct ParserError(pub Box<ErrorData>);

type TRef<'t> = &'t Token<'t>;
type TList<'t> = &'t [Token<'t>];
type TResult<'t, O> = Result<(TList<'t>, O), ParserError>;

pub fn print_error(prefix: &str, err: &ParserError) {
    let err = &err.0;
    eprintln!(
        "{}: {}\n  --> {:?} @ {:?}:{}",
        prefix, err.message, err.token, err.file_name, err.line_number
    );
}

fn make_error<'t, O, S: AsRef<str>>(token: TRef<'t>, message: S) -> Result<O, ParserError> {
    Err(ParserError(Box::new(ErrorData {
        message: message.as_ref().into(),
        token: String::from_utf8_lossy(token.text.0).into(),
        file_name: String::from_utf8_lossy(token.file_name.0).into(),
        line_number: token.line_number,
    })))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NodeKind {
    Token,
    Parens,
    Braces,
    Brackets,
    Group,
    // Second pass nodes
    Discard,
    Attribute,
    Alignas,
    Atomic,
    Typeof,
    Struct,
    Union,
    Enum,
}

#[derive(Debug, Clone)]
struct Node<'t> {
    kind: NodeKind,
    token: TRef<'t>,
    items: Vec<Node<'t>>,
    ends_with_semicolon: bool,
    name: Option<TRef<'t>>,
    has_definition: bool,
}

impl<'t> Node<'t> {
    pub fn new(kind: NodeKind, token: TRef<'t>) -> Self {
        Self {
            kind,
            token,
            items: Vec::new(),
            ends_with_semicolon: false,
            name: None,
            has_definition: false,
        }
    }

    pub fn is_token(&self, kind: TKind) -> bool {
        self.kind == NodeKind::Token && self.token.kind == kind
    }
}

fn parse_group(mut tokens: TList) -> TResult<Node> {
    let mut list = Node::new(NodeKind::Group, &tokens[0]);
    let mut group = Node::new(NodeKind::Group, &tokens[0]);
    while !tokens.is_empty() {
        match tokens[0].kind {
            TKind::Semicolon => {
                group.ends_with_semicolon = true;
                list.items.push(group);
                tokens = &tokens[1..];
                group = Node::new(NodeKind::Group, &tokens[0]);
                continue;
            }
            TKind::OpenBrace => {
                let (rest, mut node) = parse_group(&tokens[1..])?;
                if rest[0].kind != TKind::CloseBrace {
                    return make_error(&rest[0], "expected '}'");
                }
                assert_eq!(node.kind, NodeKind::Group);
                node.token = &tokens[0];
                node.kind = NodeKind::Braces;
                tokens = &rest[1..];

                // Check for a case where open brace follows closed paren "){" or
                // close bracket "]{". It should end the current group after the braces
                // This happens in the function definitions and in control flow blocks
                let has_parens = match group.items.last() {
                    Some(ref n) if n.kind == NodeKind::Parens => true,
                    Some(ref n) if n.kind == NodeKind::Brackets => true,
                    _ => false,
                };
                group.items.push(node);
                if has_parens {
                    list.items.push(group);
                    group = Node::new(NodeKind::Group, &tokens[0]);
                }
            }
            TKind::OpenParen => {
                let (rest, mut node) = parse_group(&tokens[1..])?;
                if rest[0].kind != TKind::CloseParen {
                    return make_error(&tokens[0], "expected ')'");
                }
                assert_eq!(node.kind, NodeKind::Group);
                node.token = &tokens[0];
                node.kind = NodeKind::Parens;
                tokens = &rest[1..];
                group.items.push(node);
            }
            TKind::OpenBracket => {
                let (rest, mut node) = parse_group(&tokens[1..])?;
                if rest[0].kind != TKind::CloseBracket {
                    return make_error(&tokens[0], "expected ']'");
                }
                assert_eq!(node.kind, NodeKind::Group);
                node.token = &tokens[0];
                node.kind = NodeKind::Brackets;
                tokens = &rest[1..];
                group.items.push(node);
            }
            TKind::CloseBrace | TKind::CloseParen | TKind::CloseBracket | TKind::EndOfFile => {
                break;
            }
            _ => {
                let node = Node::new(NodeKind::Token, &tokens[0]);
                group.items.push(node);
                tokens = &tokens[1..];
            }
        }
    }

    if !group.items.is_empty() {
        // Shortcut to avoid nested groups of size 1
        if list.items.is_empty() {
            return Ok((tokens, group));
        } else {
            list.items.push(group);
        }
    }

    Ok((tokens, list))
}

fn merge_specifiers(node: &mut Node) {
    let mut iter = node.items.iter_mut().peekable();
    while let Some(item) = iter.next() {
        if item.is_token(TKind::Attribute)
            || item.is_token(TKind::Alignas)
            || item.is_token(TKind::Atomic)
            || item.is_token(TKind::Typeof)
        {
            if let Some(parens) = iter.peek_mut() {
                if parens.kind == NodeKind::Parens {
                    parens.kind = NodeKind::Discard;
                    if item.token.kind == TKind::Attribute {
                        if let Some(mut parens) = parens.items.pop() {
                            core::mem::swap(&mut parens.items, &mut item.items);
                        }
                    } else {
                        core::mem::swap(&mut parens.items, &mut item.items);
                    }
                    let _ = iter.next();
                }
            }
            item.kind = match item.token.kind {
                TKind::Attribute => NodeKind::Attribute,
                TKind::Alignas => NodeKind::Alignas,
                TKind::Atomic => NodeKind::Atomic,
                TKind::Typeof => NodeKind::Typeof,
                _ => unreachable!(),
            };
        }
    }
    node.items.retain(|n| n.kind != NodeKind::Discard);
}

fn merge_structs(node: &mut Node) {
    let mut iter = node.items.iter_mut().peekable();
    while let Some(item) = iter.next() {
        if item.is_token(TKind::Struct) || item.is_token(TKind::Union) || item.is_token(TKind::Enum)
        {
            // Skip all attributes before the name
            while let Some(attr) = iter.peek_mut() {
                if attr.kind != NodeKind::Attribute {
                    break;
                }
                let _ = iter.next();
            }
            if let Some(name) = iter.peek_mut() {
                if name.is_token(TKind::Ident) {
                    item.name = Some(name.token);
                    name.kind = NodeKind::Discard;
                    let _ = iter.next();
                }
            }
            if let Some(block) = iter.peek_mut() {
                if block.kind == NodeKind::Braces {
                    item.has_definition = true;
                    core::mem::swap(&mut item.items, &mut block.items);
                    block.kind = NodeKind::Discard;
                    let _ = iter.next();
                }
            }
            item.kind = match item.token.kind {
                TKind::Struct => NodeKind::Struct,
                TKind::Union => NodeKind::Union,
                TKind::Enum => NodeKind::Enum,
                _ => unreachable!(),
            };
        }
    }
    node.items.retain(|n| n.kind != NodeKind::Discard);
}

fn parse_const_integer<'t>(expr: &mut [Node<'t>]) -> Option<IntConstExpr<'t>> {
    if expr.is_empty() {
        return None;
    }
    if expr.len() == 1 && expr[0].is_token(TKind::Number) {
        if let Ok(value) = expr[0].token.text.parse() {
            return Some(IntConstExpr::Value(value));
        }
    }
    Some(IntConstExpr::Expression(expr.to_vec()))
}

fn parse_attribute<'t>(node: &mut Node<'t>, attributes: &mut Vec<Attribute<'t>>) {
    assert_eq!(node.kind, NodeKind::Attribute);
    for attr in node.items.split_mut(|n| n.is_token(TKind::Comma)) {
        if !attr.is_empty() && attr[0].is_token(TKind::Ident) {
            let ident = attr[0].token.text.trim_char(b'_');
            if ident.0 == b"packed" {
                attributes.push(Attribute::Packed);
            } else if ident.0 == b"aligned" {
                if let Some(parens) = attr.get_mut(1) {
                    if parens.kind == NodeKind::Parens {
                        let expr = parse_const_integer(&mut parens.items);
                        if let Some(expr) = expr {
                            attributes.push(Attribute::Aligned(expr));
                        }
                    }
                }
            } else {
                attributes.push(Attribute::Unknown(attr[0].token));
            }
        }
    }
    node.kind = NodeKind::Discard;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PrimitiveKind {
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
struct StructUnionType<'t> {
    id: u64,
    name: Option<TRef<'t>>,
    members: Vec<DeclType<'t>>,
    has_definition: bool,
}

#[derive(Debug, Clone)]
struct EnumVariant<'t> {
    name: TRef<'t>,
    value: Option<IntConstExpr<'t>>,
}

#[derive(Debug, Clone)]
struct EnumType<'t> {
    id: u64,
    name: Option<TRef<'t>>,
    members: Vec<EnumVariant<'t>>,
    has_definition: bool,
}

#[derive(Debug, Clone)]
enum TypeKind<'t> {
    Primitive {
        kind: PrimitiveKind,
        is_signed: bool,
        is_unsigned: bool,
    },
    Struct {
        st: StructUnionType<'t>,
    },
    Union {
        un: StructUnionType<'t>,
    },
    Enum {
        en: EnumType<'t>,
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
enum IntConstExpr<'t> {
    Value(i64),
    Expression(Vec<Node<'t>>),
}

#[derive(Debug, Clone)]
enum Attribute<'t> {
    Packed,
    Aligned(IntConstExpr<'t>),
    Unknown(TRef<'t>),
}

#[derive(Debug, Clone)]
struct DeclType<'t> {
    id: u64,
    token: TRef<'t>,
    name: Option<TRef<'t>>,
    attributes: Vec<Attribute<'t>>,
    type_kind: TypeKind<'t>,
    is_typedef: bool,
    is_typeof: bool,
    is_const: bool,
    is_volatile: bool,
    is_static: bool,
    is_inline: bool,
    is_noreturn: bool,
    is_bit_field: Option<IntConstExpr<'t>>,
}

impl<'t> DeclType<'t> {
    pub fn new(token: TRef<'t>, id: u64) -> Self {
        Self {
            id,
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
        }
    }
}

pub struct Parser<'t> {
    next_id: u64,
    known_types: FastHashSet<ByteStr<'t>>,
    typedef: Vec<DeclType<'t>>,
    structs: Vec<StructUnionType<'t>>,
    unions: Vec<StructUnionType<'t>>,
    enums: Vec<EnumType<'t>>,
    functions: Vec<DeclType<'t>>,
    is_function_scope: bool,
}

impl<'t> Parser<'t> {
    pub fn new() -> Self {
        Self {
            next_id: 0,
            known_types: FastHashSet::default(),
            typedef: Default::default(),
            structs: Default::default(),
            unions: Default::default(),
            enums: Default::default(),
            functions: Default::default(),
            is_function_scope: false,
        }
    }

    fn next_id(&mut self) -> u64 {
        let id = self.next_id;
        self.next_id += 1;
        id
    }

    fn parse_struct_union(
        &mut self,
        node: &mut Node<'t>,
    ) -> Result<StructUnionType<'t>, ParserError> {
        let mut members = Vec::new();
        for item in node.items.iter_mut() {
            self.parse_declarations(item, &mut members)?;
        }
        let st = StructUnionType {
            id: self.next_id(),
            name: node.name,
            members,
            has_definition: node.has_definition,
        };
        if !self.is_function_scope && st.has_definition {
            if node.kind == NodeKind::Struct {
                self.structs.push(st.clone());
            } else {
                self.unions.push(st.clone());
            }
        }
        Ok(st)
    }

    fn parse_enum(&mut self, node: &mut Node<'t>) -> Result<EnumType<'t>, ParserError> {
        let mut members = Vec::new();
        for items in node.items.split_mut(|n| n.is_token(TKind::Comma)) {
            if items.is_empty() {
                continue;
            }
            if !items[0].is_token(TKind::Ident) {
                return make_error(items[0].token, "Expected an identifier");
            }
            let mut variant = EnumVariant {
                name: items[0].token,
                value: None,
            };
            if items.len() > 1 {
                if items[1].is_token(TKind::Assignment) {
                    variant.value = parse_const_integer(&mut items[2..]);
                } else {
                    return make_error(items[1].token, "Expected '='");
                }
            }
            members.push(variant);
        }
        let en = EnumType {
            id: self.next_id(),
            name: node.name,
            members,
            has_definition: node.has_definition,
        };
        if !self.is_function_scope && en.has_definition {
            self.enums.push(en.clone());
        }
        Ok(en)
    }

    fn parse_base_type<'a>(
        &mut self,
        mut items: &'a mut [Node<'t>],
    ) -> Result<(&'a mut [Node<'t>], DeclType<'t>), ParserError> {
        let mut decl = DeclType::new(items[0].token, self.next_id());
        while !items.is_empty() {
            let item = &mut items[0];
            match item.kind {
                NodeKind::Token => match item.token.kind {
                    TKind::Typedef => {
                        decl.is_typedef = true;
                    }
                    TKind::Extern => {}
                    TKind::Static => {
                        decl.is_static = true;
                    }
                    TKind::ThreadLocal => {
                        return make_error(item.token, "Thread local storage not supported");
                    }
                    TKind::Auto => {
                        return make_error(item.token, "Auto storage not supported");
                    }
                    TKind::Register => {
                        return make_error(item.token, "Register storage not supported");
                    }
                    TKind::Void => {
                        if !decl.type_kind.is_void() {
                            return make_error(item.token, "Unexpected type declaration");
                        }
                    }
                    TKind::CharType => {
                        if let TypeKind::Primitive { kind, .. } = &mut decl.type_kind {
                            *kind = PrimitiveKind::Char;
                        } else {
                            return make_error(item.token, "Unexpected type declaration");
                        }
                    }
                    TKind::Short => {
                        if let TypeKind::Primitive { kind, .. } = &mut decl.type_kind {
                            *kind = PrimitiveKind::Short;
                        } else {
                            return make_error(item.token, "Unexpected type declaration");
                        }
                    }
                    TKind::Int => {
                        if let TypeKind::Primitive { kind, .. } = &mut decl.type_kind {
                            if *kind == PrimitiveKind::Void {
                                *kind = PrimitiveKind::Int;
                            }
                        } else {
                            return make_error(item.token, "Unexpected type declaration");
                        }
                    }
                    TKind::Long => {
                        if let TypeKind::Primitive { kind, .. } = &mut decl.type_kind {
                            if *kind == PrimitiveKind::Long {
                                *kind = PrimitiveKind::LongLong;
                            } else {
                                *kind = PrimitiveKind::Long;
                            }
                        } else {
                            return make_error(item.token, "Unexpected type declaration");
                        }
                    }
                    TKind::Float => {
                        if let TypeKind::Primitive { kind, .. } = &mut decl.type_kind {
                            *kind = PrimitiveKind::Float;
                        } else {
                            return make_error(item.token, "Unexpected type declaration");
                        }
                    }
                    TKind::Double => {
                        if let TypeKind::Primitive { kind, .. } = &mut decl.type_kind {
                            if *kind == PrimitiveKind::Long {
                                *kind = PrimitiveKind::LongDouble;
                            } else {
                                *kind = PrimitiveKind::Double;
                            }
                        } else {
                            return make_error(item.token, "Unexpected type declaration");
                        }
                    }
                    TKind::Signed => {
                        if let TypeKind::Primitive {
                            kind, is_signed, ..
                        } = &mut decl.type_kind
                        {
                            if *kind == PrimitiveKind::Void {
                                *kind = PrimitiveKind::Int;
                            }
                            *is_signed = true;
                        } else {
                            return make_error(item.token, "Unexpected type declaration");
                        }
                    }
                    TKind::Unsigned => {
                        if let TypeKind::Primitive {
                            kind, is_unsigned, ..
                        } = &mut decl.type_kind
                        {
                            if *kind == PrimitiveKind::Void {
                                *kind = PrimitiveKind::Int;
                            }
                            *is_unsigned = true;
                        } else {
                            return make_error(item.token, "Unexpected type declaration");
                        }
                    }
                    TKind::Bool => {
                        if let TypeKind::Primitive { kind, .. } = &mut decl.type_kind {
                            *kind = PrimitiveKind::Bool;
                        } else {
                            return make_error(item.token, "Unexpected type declaration");
                        }
                    }
                    TKind::Complex => {
                        return make_error(item.token, "Complex type not supported");
                    }
                    TKind::Int128 => {
                        if let TypeKind::Primitive { kind, .. } = &mut decl.type_kind {
                            *kind = PrimitiveKind::Int128;
                        } else {
                            return make_error(item.token, "Unexpected type declaration");
                        }
                    }
                    TKind::UInt128 => {
                        if let TypeKind::Primitive {
                            kind, is_unsigned, ..
                        } = &mut decl.type_kind
                        {
                            *kind = PrimitiveKind::Int128;
                            *is_unsigned = true;
                        } else {
                            return make_error(item.token, "Unexpected type declaration");
                        }
                    }
                    TKind::Const => {
                        decl.is_const = true;
                    }
                    TKind::Restrict => {}
                    TKind::Volatile => {
                        decl.is_volatile = true;
                    }
                    TKind::Inline => {
                        decl.is_inline = true;
                    }
                    TKind::Noreturn => {
                        decl.is_noreturn = true;
                    }
                    TKind::Extension => {
                        decl.attributes.push(Attribute::Unknown(item.token));
                    }
                    TKind::BuiltinVaList => {
                        decl.type_kind = TypeKind::Special { name: item.token };
                    }
                    TKind::Ident => {
                        if !self.known_types.contains(&item.token.text) {
                            // Found a new type or a variable
                            break;
                        }
                        decl.type_kind = TypeKind::Typedef { name: item.token };
                    }
                    TKind::Asterisk => {
                        // Pointer is not part of the base type
                        break;
                    }
                    TKind::Colon => {
                        // Bit field is not part of the base type
                        break;
                    }
                    _ => {
                        return make_error(item.token, "Unexpected token in a type declaration");
                    }
                },
                NodeKind::Attribute => {
                    parse_attribute(item, &mut decl.attributes);
                }
                NodeKind::Alignas => {
                    decl.attributes.push(Attribute::Unknown(item.token));
                }
                NodeKind::Atomic => {
                    return make_error(item.token, "Atomic types are not supported");
                }
                NodeKind::Typeof => {
                    decl.is_typeof = true;
                }
                NodeKind::Struct => {
                    if !decl.type_kind.is_void() {
                        return make_error(item.token, "Unexpected type declaration");
                    }
                    let st = self.parse_struct_union(item)?;
                    decl.type_kind = TypeKind::Struct { st };
                }
                NodeKind::Union => {
                    if !decl.type_kind.is_void() {
                        return make_error(item.token, "Unexpected type declaration");
                    }
                    let un = self.parse_struct_union(item)?;
                    decl.type_kind = TypeKind::Union { un };
                }
                NodeKind::Enum => {
                    if !decl.type_kind.is_void() {
                        return make_error(item.token, "Unexpected type declaration");
                    }
                    let en = self.parse_enum(item)?;
                    decl.type_kind = TypeKind::Enum { en };
                }
                NodeKind::Parens | NodeKind::Brackets | NodeKind::Braces => {
                    // Parens are not part of the base type
                    break;
                }
                _ => {
                    return make_error(item.token, "Unexpected node in a type declaration");
                }
            }
            if !(item.kind == NodeKind::Struct
                || item.kind == NodeKind::Union
                || item.kind == NodeKind::Enum)
            {
                item.kind = NodeKind::Discard;
            }
            items = &mut items[1..];
        }
        Ok((items, decl))
    }

    fn parse_function_arguments(
        &mut self,
        decl: &mut DeclType<'t>,
        node: &mut Node<'t>,
    ) -> Result<(), ParserError> {
        merge_specifiers(node);
        merge_structs(node);

        let mut args = Vec::new();
        let mut is_variadic = false;

        let old_scope = self.is_function_scope;
        self.is_function_scope = true;

        for items in node.items.split_mut(|n| n.is_token(TKind::Comma)) {
            if items.is_empty() {
                break;
            }
            if items.len() == 1 {
                if items[0].is_token(TKind::Void) {
                    break;
                }
                if items[0].is_token(TKind::Ellipsis) {
                    is_variadic = true;
                    break;
                }
            }
            let (rest, mut base) = self.parse_base_type(items)?;
            if !rest.is_empty() {
                self.parse_declarator(&mut base, rest)?;
            }
            args.push(base);
        }

        self.is_function_scope = old_scope;
        decl.type_kind.make_function(args, is_variadic);
        Ok(())
    }

    fn parse_type_suffix(
        &mut self,
        decl: &mut DeclType<'t>,
        mut items: &mut [Node<'t>],
    ) -> Result<(), ParserError> {
        if !items.is_empty() && items[0].kind == NodeKind::Parens {
            // Function arguments
            self.parse_function_arguments(decl, &mut items[0])?;
            items = &mut items[1..];
        } else {
            while !items.is_empty() && items[0].kind == NodeKind::Brackets {
                // Array declaration
                let size = parse_const_integer(&mut items[0].items);
                decl.type_kind.make_array(size);
                items[0].kind = NodeKind::Discard;
                items = &mut items[1..];
            }
        }
        if !items.is_empty() && items[0].is_token(TKind::Colon) {
            // Bit field
            let bits = parse_const_integer(&mut items[1..]);
            if bits.is_none() {
                return make_error(items[0].token, "Expected a number after ':'");
            }
            decl.is_bit_field = bits;
        } else {
            while !items.is_empty() && items[0].kind == NodeKind::Attribute {
                parse_attribute(&mut items[0], &mut decl.attributes);
                items = &mut items[1..];
            }
            if !items.is_empty() {
                if items[0].kind == NodeKind::Braces {
                    // Function body
                    items[0].kind = NodeKind::Discard;
                } else if items[0].is_token(TKind::Assignment) {
                    // Assignment
                } else {
                    return make_error(
                        items[0].token,
                        "Unexpected token at the end of type declaration",
                    );
                }
            }
        }
        Ok(())
    }

    fn parse_pointers<'a>(
        &mut self,
        decl: &mut DeclType<'t>,
        mut items: &'a mut [Node<'t>],
    ) -> Result<&'a mut [Node<'t>], ParserError> {
        while !items.is_empty() {
            if items[0].is_token(TKind::Asterisk) {
                decl.type_kind.make_pointer();
            } else if items[0].is_token(TKind::Const) {
                if let TypeKind::Pointer { is_const, .. } = &mut decl.type_kind {
                    *is_const = true;
                } else {
                    return make_error(items[0].token, "Unexpected const");
                }
            } else if items[0].is_token(TKind::Volatile) {
                if let TypeKind::Pointer { is_volatile, .. } = &mut decl.type_kind {
                    *is_volatile = true;
                } else {
                    return make_error(items[0].token, "Unexpected volatile");
                }
            } else if items[0].is_token(TKind::Restrict) {
                // ignore
            } else {
                break;
            }
            items[0].kind = NodeKind::Discard;
            items = &mut items[1..];
        }
        Ok(items)
    }

    fn parse_declarator(
        &mut self,
        decl: &mut DeclType<'t>,
        mut items: &mut [Node<'t>],
    ) -> Result<(), ParserError> {
        items = self.parse_pointers(decl, items)?;

        while !items.is_empty() && items[0].kind == NodeKind::Attribute {
            parse_attribute(&mut items[0], &mut decl.attributes);
            items = &mut items[1..];
        }

        if !items.is_empty() {
            if items[0].kind == NodeKind::Parens {
                self.parse_type_suffix(decl, &mut items[1..])?;
                return self.parse_declarator(decl, &mut items[0].items);
            }

            if items[0].is_token(TKind::Ident) {
                decl.name = Some(items[0].token);
                items[0].kind = NodeKind::Discard;
                items = &mut items[1..];
            }
        }

        self.parse_type_suffix(decl, items)?;
        Ok(())
    }

    fn parse_declarations(
        &mut self,
        node: &mut Node<'t>,
        result: &mut Vec<DeclType<'t>>,
    ) -> Result<(), ParserError> {
        if node.items.is_empty() || node.items[0].is_token(TKind::StaticAssert) {
            node.kind = NodeKind::Discard;
            return Ok(());
        }

        merge_specifiers(node);
        merge_structs(node);

        let items: &mut [Node<'t>] = &mut node.items;
        let (rest, base) = self.parse_base_type(items)?;
        if !rest.is_empty() {
            for items in rest.split_mut(|n| n.is_token(TKind::Comma)) {
                if items.is_empty() {
                    return make_error(node.token, "Empty declaration");
                }
                let mut decl = base.clone();
                decl.id = self.next_id();
                self.parse_declarator(&mut decl, items)?;
                result.push(decl);
            }
        } else if base.is_typedef {
            return make_error(node.token, "Missing typedef name");
        }
        Ok(())
    }

    fn parse_global_declarations(&mut self, node: &mut Node<'t>) -> Result<(), ParserError> {
        let mut declarations = Vec::new();
        self.parse_declarations(node, &mut declarations)?;
        for decl in declarations {
            if decl.is_typedef {
                if let Some(name) = decl.name {
                    self.known_types.insert(name.text);
                    self.typedef.push(decl);
                } else {
                    return make_error(node.token, "Missing typedef name");
                }
            } else {
                if matches!(decl.type_kind, TypeKind::Function { .. })
                    && !decl.is_static
                    && !decl.is_inline
                {
                    if decl.name.is_some() {
                        self.functions.push(decl);
                    } else {
                        return make_error(node.token, "Missing function name");
                    }
                }
            }
        }
        Ok(())
    }

    fn parse_top_level(&mut self, root_node: &mut Node<'t>) {
        for statement in root_node.items.iter_mut() {
            if let Err(err) = self.parse_global_declarations(statement) {
                print_error("warning", &err);
            }
        }
    }

    pub fn parse(&mut self, tokens: TList<'t>) -> Result<(), ParserError> {
        let (rest, mut node) = parse_group(tokens)?;
        if rest[0].kind != TKind::EndOfFile {
            return make_error(&rest[0], "Unexpected token at the end of the file");
        }

        if false {
            let mut out = String::new();
            print_node(&node, &mut out);
            std::fs::write("stage1.c", &out).unwrap();
        }

        self.parse_top_level(&mut node);

        // Save 12% execution time by not freeing the memory
        std::mem::forget(node);
        Ok(())
    }

    pub fn generate_queries(&self, output: &mut String) {
        for st in &self.structs {
            output.push_str(&format!(
                "struct {} {:?} {{",
                st.id,
                st.name.map(|t| t.text)
            ));
            for member in &st.members {
                output.push_str(&format!("  {:?}", member.name.map(|t| t.text)));
            }
            output.push_str("};\n");
        }
    }
}

fn print_node(node: &Node, out: &mut String) {
    match node.kind {
        NodeKind::Token => print_token(node.token, out),
        NodeKind::Parens => {
            out.push('(');
            for n in &node.items {
                print_node(n, out);
                if n.ends_with_semicolon {
                    out.push_str("; ");
                }
            }
            out.push(')');
        }
        NodeKind::Braces => {
            out.push_str("\n{\n");
            for n in &node.items {
                print_node(n, out);
                if n.ends_with_semicolon {
                    out.push_str(";\n");
                }
            }
            out.push_str("}\n");
        }
        NodeKind::Brackets => {
            out.push('[');
            for n in &node.items {
                print_node(n, out);
                if n.ends_with_semicolon {
                    out.push_str("; ");
                }
            }
            out.push(']');
        }
        NodeKind::Group => {
            for n in &node.items {
                print_node(n, out);
                if n.ends_with_semicolon {
                    out.push_str(";\n");
                }
            }
        }
        NodeKind::Discard => {
            // skip
        }
        _ => {
            out.push_str(&format!("***{:?}***\n", node.kind));
        }
    }
}

fn print_token(token: TRef, out: &mut String) {
    if token.has_space || token.start_of_line {
        out.push(' ');
    }

    if token.kind == TKind::String {
        out.push('\"');
    } else if token.kind == TKind::Char {
        out.push('\'');
    }

    let text = String::from_utf8_lossy(token.text.0);
    out.push_str(&text);

    if token.kind == TKind::String {
        out.push('\"');
    } else if token.kind == TKind::Char {
        out.push('\'');
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenizer::Tokenizer;
    use crate::utils::ByteStr;

    fn tokenize_code(code: &str) -> Vec<Token> {
        let mut tokenizer = Tokenizer::new(ByteStr(code.as_bytes()));
        tokenizer.tokenize()
    }

    fn parse_code(tokens: TList) -> Node {
        let (rest, node) = parse_group(&tokens).unwrap();
        assert_eq!(rest[0].kind, TKind::EndOfFile);
        node
    }

    #[test]
    fn test_attributes() {
        let tokens = tokenize_code("__attribute__((,packed,aligned(4)))");
        let mut node = parse_code(&tokens);
        merge_specifiers(&mut node);
        assert_eq!(node.items.len(), 1);
        let mut attrs = Vec::new();
        parse_attribute(&mut node.items[0], &mut attrs);
        assert_eq!(attrs.len(), 2);
        assert!(matches!(attrs[0], Attribute::Packed));
        assert!(matches!(
            attrs[1],
            Attribute::Aligned(IntConstExpr::Value(4))
        ));
    }

    #[test]
    fn test_specifiers() {
        let tokens = tokenize_code("typeof(1) _Atomic _Alignas(int)");
        let mut node = parse_code(&tokens);
        merge_specifiers(&mut node);
        assert_eq!(node.items.len(), 3);
        assert_eq!(node.items[0].kind, NodeKind::Typeof);
        assert_eq!(node.items[0].items.len(), 1);
        assert_eq!(node.items[1].kind, NodeKind::Atomic);
        assert_eq!(node.items[1].items.len(), 0);
        assert_eq!(node.items[2].kind, NodeKind::Alignas);
        assert_eq!(node.items[2].items.len(), 1);
        assert_eq!(node.items[2].items[0].kind, NodeKind::Token);
    }
}
