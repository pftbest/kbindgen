use crate::tokenizer::{TKind, Token};

#[allow(dead_code)]
#[derive(Debug)]
struct TError<'t> {
    token: TRef<'t>,
    message: String,
}

type TRef<'t> = &'t Token<'t>;
type TList<'t> = &'t [Token<'t>];
type TResult<'t, O> = Result<(TList<'t>, O), TError<'t>>;

type NList<'t> = &'t [Node<'t>];
type NResult<'t, O> = Result<(NList<'t>, O), TError<'t>>;

fn make_error<'t, O, S: AsRef<str>>(token: TRef<'t>, message: S) -> Result<O, TError<'t>> {
    Err(TError {
        token,
        message: message.as_ref().to_owned(),
    })
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NodeKind {
    Token,
    Parens,
    Braces,
    Brackets,
    Group,
    // Second pass nodes
    Struct,
    Union,
    Enum,
    Typedef,
    FunctionDef,
}

#[derive(Debug)]
struct Node<'t> {
    kind: NodeKind,
    token: TRef<'t>,
    items: Vec<Node<'t>>,
    ends_with_semicolon: bool,
    has_comma: bool,
    has_assignment: bool,
    has_typedef: bool,
    name: Option<TRef<'t>>,
}

impl<'t> Node<'t> {
    pub fn new(kind: NodeKind, token: TRef<'t>) -> Self {
        Self {
            kind,
            token,
            items: Vec::new(),
            ends_with_semicolon: false,
            has_comma: false,
            has_assignment: false,
            has_typedef: false,
            name: None,
        }
    }

    pub fn is_token(&self, kind: TKind) -> bool {
        self.kind == NodeKind::Token && self.token.kind == kind
    }

    pub fn is_node(&self, kind: NodeKind) -> bool {
        self.kind == kind
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
                node.kind = NodeKind::Braces;
                tokens = &rest[1..];

                // Check for a case where open brace follows closed paren "){"
                // It should end the current group after the braces
                // This happens in the function definitions and in control flow blocks
                let has_parens = match group.items.last() {
                    Some(ref n) if n.kind == NodeKind::Parens => true,
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
                node.kind = NodeKind::Brackets;
                tokens = &rest[1..];
                group.items.push(node);
            }
            TKind::CloseBrace | TKind::CloseParen | TKind::CloseBracket | TKind::EndOfFile => {
                break;
            }
            kind => {
                match kind {
                    TKind::Comma => {
                        group.has_comma = true;
                    }
                    TKind::Assignment => {
                        group.has_assignment = true;
                    }
                    TKind::Typedef => {
                        group.has_typedef = true;
                    }
                    _ => {}
                }
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

#[derive(Debug)]
struct BaseType<'t> {
    specs: Vec<TRef<'t>>,
    st: Option<StructType<'t>>,
    un: Option<StructType<'t>>,
    en: Option<EnumType<'t>>,
    attrs: Vec<Attribute<'t>>,
}

#[derive(Debug)]
struct StructType<'t> {
    name: Option<TRef<'t>>,
    members: Option<Vec<()>>,
}

#[derive(Debug)]
struct EnumType<'t> {
    name: Option<TRef<'t>>,
    members: Option<Vec<()>>,
}

#[allow(dead_code)]
#[derive(Debug)]
struct Attribute<'t> {
    name: TRef<'t>,
    args: Option<Vec<()>>,
}

fn expect_node(nodes: NList, kind: NodeKind) -> Result<&Node, TError> {
    if let Some(node) = nodes.first() {
        if node.kind != kind {
            return make_error(
                node.token,
                format!("expected {:?} but got {:?}", kind, node.kind),
            );
        }
        Ok(node)
    } else {
        panic!("empty node list");
    }
}

fn merge_specifiers(node: &mut Node) {
    let mut old_items = Vec::with_capacity(node.items.len());
    core::mem::swap(&mut node.items, &mut old_items);
    let mut iter = old_items.into_iter().peekable();
    while let Some(mut item) = iter.next() {
        if item.is_token(TKind::Struct) || item.is_token(TKind::Union) || item.is_token(TKind::Enum) {
            if let Some(name) = iter.peek() {
                if name.is_token(TKind::Ident) {
                    item.name = Some(name.token);
                    let _ = iter.next();
                }
            }
            if let Some(block) = iter.peek() {
                if block.is_node(NodeKind::Braces) {
                    item.items = iter.next().unwrap().items;
                }
            }
            item.kind = match item.token.kind {
                TKind::Struct => NodeKind::Struct,
                TKind::Union => NodeKind::Union,
                TKind::Enum => NodeKind::Enum,
                _ => unreachable!(),
            };
        } else if item.is_token(TKind::Attribute) {
            if let Some(parens) = iter.peek() {
                if parens.is_node(NodeKind::Parens) {
                    item.items = iter.next().unwrap().items;
                    if let Some(parens) = item.items.pop() {
                        item.items = parens.items;
                    }
                }
            }
        } else {
            node.items.push(item);
        }
    }
}

fn parse_attributes<'t>(nodes: NList<'t>, attrs: &mut Vec<Attribute<'t>>) -> NResult<'t, ()> {
    let node = expect_node(nodes, NodeKind::Parens)?;
    let node = expect_node(&node.items, NodeKind::Group)?;
    let node = expect_node(&node.items, NodeKind::Parens)?;
    let node = expect_node(&node.items, NodeKind::Group)?;
    let mut items = &node.items[..];
    while !items.is_empty() {
        let name = expect_node(items, NodeKind::Token)?;
        let mut attr = Attribute {
            name: name.token,
            args: None,
        };
        items = &items[1..];
        if !items.is_empty() && items[0].is_node(NodeKind::Parens) {
            attr.args = Some(Vec::new());
            // TODO: parse arguments
            items = &items[1..];
        }
        if !items.is_empty() && items[0].is_token(TKind::Comma) {
            items = &items[1..];
        }
        attrs.push(attr);
    }
    Ok((&nodes[1..], ()))
}

pub struct Parser {}

impl Parser {
    pub fn new() -> Self {
        Self {}
    }

    fn parse_enum_type<'t>(&mut self, mut nodes: NList<'t>) -> NResult<'t, EnumType<'t>> {
        let mut en = EnumType {
            name: None,
            members: None,
        };
        if !nodes.is_empty() && nodes[0].is_token(TKind::Ident) {
            en.name = Some(nodes[0].token);
            nodes = &nodes[1..];
        }
        if !nodes.is_empty() && nodes[0].is_node(NodeKind::Braces) {
            en.members = Some(Vec::new());
            // TODO: parse members
            nodes = &nodes[1..];
        }
        if en.name.is_none() && en.members.is_none() {
            return make_error(nodes[0].token, "bad enum type");
        }
        Ok((nodes, en))
    }

    fn parse_struct_type<'t>(&mut self, mut nodes: NList<'t>) -> NResult<'t, StructType<'t>> {
        let mut st = StructType {
            name: None,
            members: None,
        };
        if !nodes.is_empty() && nodes[0].is_token(TKind::Ident) {
            st.name = Some(nodes[0].token);
            nodes = &nodes[1..];
        }
        if !nodes.is_empty() && nodes[0].is_node(NodeKind::Braces) {
            st.members = Some(Vec::new());
            // TODO: parse members
            nodes = &nodes[1..];
        }
        if st.name.is_none() && st.members.is_none() {
            return make_error(nodes[0].token, "bad struct/union type");
        }
        Ok((nodes, st))
    }

    fn parse_base_type<'t>(&mut self, mut nodes: NList<'t>) -> NResult<'t, BaseType<'t>> {
        let mut base = BaseType {
            specs: Vec::new(),
            st: None,
            un: None,
            en: None,
            attrs: Vec::new(),
        };
        let mut prev = nodes;
        while !nodes.is_empty() {
            let node = &nodes[0];
            match node.kind {
                NodeKind::Token => {
                    let token = &node.token;
                    match token.kind {
                        TKind::Ident => {
                            base.specs.push(token);
                            prev = nodes;
                            nodes = &nodes[1..];
                        }
                        TKind::Typedef => {
                            // skip typedef keyword
                            nodes = &nodes[1..];
                        }
                        TKind::Struct => {
                            let (rest, st) = self.parse_struct_type(&nodes[1..])?;
                            base.st = Some(st);
                            nodes = rest;
                        }
                        TKind::Union => {
                            let (rest, un) = self.parse_struct_type(&nodes[1..])?;
                            base.un = Some(un);
                            nodes = rest;
                        }
                        TKind::Enum => {
                            let (rest, en) = self.parse_enum_type(&nodes[1..])?;
                            base.en = Some(en);
                            nodes = rest;
                        }
                        TKind::Asterisk => {
                            break;
                        }
                        TKind::Attribute => {
                            let (rest, _) = parse_attributes(&nodes[1..], &mut base.attrs)?;
                            nodes = rest;
                        }
                        _ => {
                            return make_error(token, "unknown token");
                        }
                    }
                }
                NodeKind::Parens => {
                    break;
                }
                NodeKind::Brackets => {
                    break;
                }
                _ => {
                    return make_error(&node.token, "unknown node");
                }
            }
        }
        Ok((nodes, base))
    }

    fn parse_typedef(&mut self, node: &mut Node) {
        let nodes = &node.items;
        let (rest, base) = self.parse_base_type(nodes).unwrap();

        // if node.has_comma || node.has_assignment {
        //     println!("warning: Comma operator in typedef is not supported for now {:#?}", node.items[0].token);
        //     return;
        // }
        if let Some(ref st) = base.st {
            if let Some(name) = st.name {
                print!("[struct {:?}] ", name.text);
            } else {
                print!("[struct anon] ");
            }
        }
        if let Some(ref st) = base.un {
            if let Some(name) = st.name {
                print!("[union {:?}] ", name.text);
            } else {
                print!("[union anon] ");
            }
        }
        if let Some(ref st) = base.en {
            if let Some(name) = st.name {
                print!("[enum {:?}] ", name.text);
            } else {
                print!("[enum anon] ");
            }
        }
        for item in base.specs.iter() {
            print!("{:?} ", item.text);
        }
        println!();
    }

    fn parse_function_def(&mut self, _node: &mut Node) {}

    fn parse_declaration(&mut self, node: &mut Node) {
        merge_specifiers(node);
        if node.has_typedef {
            node.kind = NodeKind::Typedef;
            self.parse_typedef(node);
        } else if !node.ends_with_semicolon {
            node.kind = NodeKind::FunctionDef;
            self.parse_function_def(node);
        } else {
        }
    }

    fn parse_top_level(&mut self, root_node: &mut Node) {
        assert_eq!(root_node.kind, NodeKind::Group);
        for statement in root_node.items.iter_mut() {
            self.parse_declaration(statement);
        }
    }

    pub fn parse(&mut self, tokens: TList) {
        let (rest, mut node) = parse_group(tokens).unwrap();
        assert_eq!(rest[0].kind, TKind::EndOfFile);

        let mut out = String::new();
        print_node(&node, &mut out);
        std::fs::write("out.c", &out).unwrap();

        self.parse_top_level(&mut node);
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

    #[test]
    fn test_attributes() {
        let mut tokenizer = Tokenizer::new(ByteStr(b"__attribute__((packed(5),))"));
        let tokens = tokenizer.tokenize();
        let (rest, node) = parse_group(&tokens).unwrap();
        assert_eq!(rest[0].kind, TKind::EndOfFile);
        let mut attrs = Vec::new();
        let (rest, _) = parse_attributes(&node.items[1..], &mut attrs).unwrap();
        assert_eq!(rest.len(), 0);
        assert_eq!(attrs.len(), 1);
        assert_eq!(attrs[0].name.text, ByteStr(b"packed"));
        assert!(attrs[0].args.is_some());

        let mut tokenizer = Tokenizer::new(ByteStr(b"__attribute__((packed,aligned(4)))"));
        let tokens = tokenizer.tokenize();
        let (rest, node) = parse_group(&tokens).unwrap();
        assert_eq!(rest[0].kind, TKind::EndOfFile);
        let mut attrs = Vec::new();
        let (rest, _) = parse_attributes(&node.items[1..], &mut attrs).unwrap();
        assert_eq!(rest.len(), 0);
        assert_eq!(attrs.len(), 2);
        assert_eq!(attrs[0].name.text, ByteStr(b"packed"));
        assert!(attrs[0].args.is_none());
        assert_eq!(attrs[1].name.text, ByteStr(b"aligned"));
        assert!(attrs[1].args.is_some());
    }
}
