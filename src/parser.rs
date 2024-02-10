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
}

fn parse_group(mut tokens: TList) -> TResult<Node> {
    let mut list = Vec::new();
    let mut group = Vec::new();
    while !tokens.is_empty() {
        match tokens[0].kind {
            TKind::Semicolon => {
                list.push(Node {
                    kind: NodeKind::Group,
                    token: &tokens[0],
                    items: group,
                    ends_with_semicolon: true,
                });
                group = Vec::new();
                tokens = &tokens[1..];
                continue;
            }
            TKind::OpenBrace => {
                let (rest, node) = parse_group(&tokens[1..])?;
                if rest[0].kind != TKind::CloseBrace {
                    return make_error(&rest[0], "expected '}'");
                }
                tokens = &rest[1..];
                group.push(Node {
                    kind: NodeKind::Braces,
                    token: &rest[0],
                    items: vec![node],
                    ends_with_semicolon: false,
                });

                // Check for a case where open brace follows closed paren "){"
                // It should end the current group after the braces
                // This happens in the function definitions and in control flow blocks
                if group.len() > 1 && group[group.len() - 2].kind == NodeKind::Parens {
                    list.push(Node {
                        kind: NodeKind::Group,
                        token: &rest[0],
                        items: group,
                        ends_with_semicolon: false,
                    });
                    group = Vec::new();
                }
            }
            TKind::OpenParen => {
                let (rest, node) = parse_group(&tokens[1..])?;
                if rest[0].kind != TKind::CloseParen {
                    return make_error(&tokens[0], "expected ')'");
                }
                tokens = &rest[1..];
                group.push(Node {
                    kind: NodeKind::Parens,
                    token: &rest[0],
                    items: vec![node],
                    ends_with_semicolon: false,
                });
            }
            TKind::OpenBracket => {
                let (rest, node) = parse_group(&tokens[1..])?;
                if rest[0].kind != TKind::CloseBracket {
                    return make_error(&tokens[0], "expected ']'");
                }
                tokens = &rest[1..];
                group.push(Node {
                    kind: NodeKind::Brackets,
                    token: &rest[0],
                    items: vec![node],
                    ends_with_semicolon: false,
                });
            }
            TKind::CloseBrace | TKind::CloseParen | TKind::CloseBracket | TKind::EndOfFile => {
                break;
            }
            _ => {
                group.push(Node {
                    kind: NodeKind::Token,
                    token: &tokens[0],
                    items: Vec::new(),
                    ends_with_semicolon: false,
                });
                tokens = &tokens[1..];
            }
        }
    }

    if !group.is_empty() {
        let node = Node {
            kind: NodeKind::Group,
            token: &tokens[0],
            items: group,
            ends_with_semicolon: false,
        };
        // Shortcut to avoid nested groups of size 1
        if list.is_empty() {
            return Ok((tokens, node));
        } else {
            list.push(node);
        }
    }

    let node = Node {
        kind: NodeKind::Group,
        token: &tokens[0],
        items: list,
        ends_with_semicolon: false,
    };
    Ok((tokens, node))
}

pub struct Parser {}

impl Parser {
    pub fn new() -> Self {
        Self {}
    }

    // fn parse_declaration(&mut self, node: &mut Node) {
    //     match node {
    //         &mut Node::Group {
    //             ref mut items,
    //             ends_with_semicolon,
    //         } => {

    //         },
    //         _ => {
    //             panic!("Top level declaration should be a group");
    //         },
    //     }
    // }

    fn parse_top_level(&mut self, _root_node: &mut Node) {
        //     match root_node {
        //         &mut Node::StatementList(ref mut list) => {
        //             for statement in list.iter_mut() {
        //                 self.parse_declaration(statement);
        //             }
        //         },
        //         _ => {
        //             panic!("Top level should be a list");
        //         },
        //     }
    }

    pub fn parse(&mut self, tokens: TList) {
        let (rest, mut node) = parse_group(tokens).unwrap();
        assert_eq!(rest[0].kind, TKind::EndOfFile);

        let mut out = String::new();
        print_node(&node, &mut out);
        std::fs::write("out.c", &out).unwrap();

        self.parse_top_level(&mut node);

        // use std::fmt::Write;
        // out.clear();
        // writeln!(&mut out, "{:#?}", node).unwrap();
        // std::fs::write("out.txt", &out).unwrap();
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
