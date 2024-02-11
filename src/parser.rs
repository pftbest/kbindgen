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
    Discard,
    Struct,
    Union,
    Enum,
    Attribute,
    Typedef,
    FunctionDef,
}

#[allow(dead_code)]
#[derive(Debug)]
struct Attribute<'t> {
    name: TRef<'t>,
    args: Vec<Node<'t>>,
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
    attributes: Vec<Attribute<'t>>,
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
            attributes: Vec::new(),
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

fn parse_attribute<'t>(node: Node<'t>, attributes: &mut Vec<Attribute<'t>>) {
    let mut iter = node.items.into_iter();
    while let Some(item) = iter.next() {
        if item.is_token(TKind::Ident) {
            let mut attr = Attribute {
                name: item.token,
                args: Vec::new(),
            };
            if let Some(parens) = iter.next() {
                if parens.kind == NodeKind::Parens {
                    attr.args = parens.items;
                }
            }
            attributes.push(attr);
        }
    }
}

fn merge_attributes(node: &mut Node) {
    let mut iter = node.items.iter_mut();
    while let Some(item) = iter.next() {
        if item.is_token(TKind::Attribute) {
            if let Some(parens) = iter.next() {
                if parens.kind == NodeKind::Parens {
                    parens.kind = NodeKind::Discard;
                    if let Some(parens) = parens.items.pop() {
                        parse_attribute(parens, &mut item.attributes);
                    }
                }
            }
            item.kind = NodeKind::Attribute;
        }
    }
    node.items.retain(|n| n.kind != NodeKind::Discard);
}

fn merge_structs(node: &mut Node) {
    let mut iter = node.items.iter_mut().peekable();
    while let Some(item) = iter.next() {
        if item.is_token(TKind::Struct) || item.is_token(TKind::Union) || item.is_token(TKind::Enum)
        {
            // Collect all attributes before the name
            while let Some(attr) = iter.peek_mut() {
                if attr.kind != NodeKind::Attribute {
                    break;
                }
                item.attributes.extend(attr.attributes.drain(..));
                attr.kind = NodeKind::Discard;
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
                    core::mem::swap(&mut item.items, &mut block.items);
                    block.kind = NodeKind::Discard;
                    let _ = iter.next();

                    // Collect all the attributes after the closing brace
                    while let Some(attr) = iter.peek_mut() {
                        if attr.kind != NodeKind::Attribute {
                            break;
                        }
                        item.attributes.extend(attr.attributes.drain(..));
                        attr.kind = NodeKind::Discard;
                        let _ = iter.next();
                    }
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

pub struct Parser {}

impl Parser {
    pub fn new() -> Self {
        Self {}
    }

    fn parse_base_type<'t>(&mut self, mut nodes: NList<'t>) -> NResult<'t, BaseType<'t>> {
        let mut base = BaseType {
            specs: Vec::new(),
            st: None,
            un: None,
            en: None,
        };
        // let mut prev = nodes;
        while !nodes.is_empty() {
            let node = &nodes[0];
            match node.kind {
                NodeKind::Token => {
                    let token = &node.token;
                    match token.kind {
                        TKind::Ident => {
                            base.specs.push(token);
                            // prev = nodes;
                            nodes = &nodes[1..];
                        }
                        TKind::Typedef => {
                            // skip typedef keyword
                            nodes = &nodes[1..];
                        }
                        TKind::Asterisk => {
                            break;
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
                NodeKind::Struct => {
                    nodes = &nodes[1..];
                }
                NodeKind::Union => {
                    nodes = &nodes[1..];
                }
                NodeKind::Enum => {
                    nodes = &nodes[1..];
                }
                NodeKind::Attribute => {
                    // println!("attr {:#?}", node);
                    nodes = &nodes[1..];
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
        let (_rest, base) = self.parse_base_type(nodes).unwrap();

        // if node.has_comma || node.has_assignment {
        //     println!("warning: Comma operator in typedef is not supported for now {:#?}", node.items[0].token);
        //     return;
        // }
        // if let Some(ref st) = base.st {
        //     if let Some(name) = st.name {
        //         print!("[struct {:?}] ", name.text);
        //     } else {
        //         print!("[struct anon] ");
        //     }
        // }
        // if let Some(ref st) = base.un {
        //     if let Some(name) = st.name {
        //         print!("[union {:?}] ", name.text);
        //     } else {
        //         print!("[union anon] ");
        //     }
        // }
        // if let Some(ref st) = base.en {
        //     if let Some(name) = st.name {
        //         print!("[enum {:?}] ", name.text);
        //     } else {
        //         print!("[enum anon] ");
        //     }
        // }
        // for item in base.specs.iter() {
        //     print!("{:?} ", item.text);
        // }
        // println!();
    }

    fn parse_function_def(&mut self, _node: &mut Node) {
        // println!("TODO: function definition");
    }

    fn parse_declaration(&mut self, node: &mut Node) {
        merge_attributes(node);
        merge_structs(node);
        if node.has_typedef {
            node.kind = NodeKind::Typedef;
            self.parse_typedef(node);
        } else if !node.ends_with_semicolon {
            node.kind = NodeKind::FunctionDef;
            self.parse_function_def(node);
        } else {
            // println!("TODO: variable declaration");
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
        std::fs::write("stage1.c", &out).unwrap();

        self.parse_top_level(&mut node);

        out.clear();
        print_node(&node, &mut out);
        std::fs::write("stage2.c", &out).unwrap();
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
        let tokens = tokenize_code("__attribute__((,packed,aligned(4,5)))");
        let mut node = parse_code(&tokens);

        merge_attributes(&mut node);
        let attrs = &node.items[0].attributes;
        assert_eq!(attrs.len(), 2);
        assert_eq!(attrs[0].name.text, ByteStr(b"packed"));
        assert_eq!(attrs[0].args.len(), 0);
        assert_eq!(attrs[1].name.text, ByteStr(b"aligned"));
        assert_eq!(attrs[1].args.len(), 3);
    }
}
