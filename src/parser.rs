use crate::tokenizer::{TKind, Token};

#[allow(dead_code)]
#[derive(Debug)]
struct TError<'t> {
    token: TRef<'t>,
    message: &'t str,
}

type TRef<'t> = &'t Token<'t>;
type TList<'t> = &'t [Token<'t>];
type TResult<'t, O> = Result<(TList<'t>, O), TError<'t>>;

fn make_error<'t, O>(token: TRef<'t>, message: &'t str) -> TResult<'t, O> {
    Err(TError { token, message })
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NodeKind {
    Token,
    Parens,
    Braces,
    Brackets,
    Group,
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
        }
    }

    pub fn new_nested(kind: NodeKind, token: TRef<'t>, item: Self) -> Self {
        Self {
            kind,
            token,
            items: vec![item],
            ends_with_semicolon: false,
            has_comma: false,
            has_assignment: false,
            has_typedef: false,
        }
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
                let (rest, node) = parse_group(&tokens[1..])?;
                if rest[0].kind != TKind::CloseBrace {
                    return make_error(&rest[0], "expected '}'");
                }
                let node = Node::new_nested(NodeKind::Braces, &tokens[0], node);
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
                let (rest, node) = parse_group(&tokens[1..])?;
                if rest[0].kind != TKind::CloseParen {
                    return make_error(&tokens[0], "expected ')'");
                }
                let node = Node::new_nested(NodeKind::Parens, &tokens[0], node);
                tokens = &rest[1..];
                group.items.push(node);
            }
            TKind::OpenBracket => {
                let (rest, node) = parse_group(&tokens[1..])?;
                if rest[0].kind != TKind::CloseBracket {
                    return make_error(&tokens[0], "expected ']'");
                }
                let node = Node::new_nested(NodeKind::Brackets, &tokens[0], node);
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

pub struct Parser {}

impl Parser {
    pub fn new() -> Self {
        Self {}
    }

    fn parse_typedef(&mut self, node: &mut Node) {
        if node.has_comma || node.has_assignment {
            println!("typedef {:#?}", node);
        }
    }

    fn parse_declaration(&mut self, node: &mut Node) {
        if node.has_typedef {
            self.parse_typedef(node);
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
            print_node(&node.items[0], out);
            out.push(')');
        }
        NodeKind::Braces => {
            out.push_str("\n{\n");
            print_node(&node.items[0], out);
            out.push_str("\n}\n");
        }
        NodeKind::Brackets => {
            out.push('[');
            print_node(&node.items[0], out);
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
    // use super::*;
    // use crate::tokenizer::Tokenizer;
    // use crate::utils::ByteStr;

    // #[test]
    // fn test_attributes() {
    //     let mut tokenizer = Tokenizer::new(ByteStr(b"__attribute__((packed(5),))"));
    //     let tokens = tokenizer.tokenize();
    //     let (rest, attrs) = parse_attribute(&tokens).unwrap();
    //     assert_eq!(attrs.len(), 1);
    //     assert_eq!(attrs[0].name.text, ByteStr(b"packed"));
    //     assert_eq!(attrs[0].args.len(), 1);
    //     assert_eq!(rest[0].kind, TKind::EndOfFile);

    //     let mut tokenizer = Tokenizer::new(ByteStr(b"__attribute__((packed,aligned(4)))"));
    //     let tokens = tokenizer.tokenize();
    //     let (rest, attrs) = parse_attribute(&tokens).unwrap();
    //     assert_eq!(attrs.len(), 2);
    //     assert_eq!(attrs[0].name.text, ByteStr(b"packed"));
    //     assert_eq!(attrs[0].args.len(), 0);
    //     assert_eq!(attrs[1].name.text, ByteStr(b"aligned"));
    //     assert_eq!(attrs[1].args.len(), 1);
    //     assert_eq!(rest[0].kind, TKind::EndOfFile);
    // }
}
