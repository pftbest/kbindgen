use crate::tokenizer::{TKind, Token};

#[allow(dead_code)]
#[derive(Debug)]
struct TError<'t> {
    token: TRef<'t>,
    expected: TKind,
}

type TRef<'t> = &'t Token<'t>;
type TList<'t> = &'t [Token<'t>];
type TResult<'t, O> = Result<(TList<'t>, O), TError<'t>>;

fn make_error<O>(token: TRef, expected: TKind) -> TResult<O> {
    Err(TError { token, expected })
}

enum Node<'t> {
    Token(TRef<'t>),
    Parens(Box<Node<'t>>),
    Braces(Box<Node<'t>>),
    Brackets(Box<Node<'t>>),
    Group {
        items: Vec<Node<'t>>,
        ends_with_semicolon: bool,
    },
    StatementList(Vec<Node<'t>>),
}

impl<'t> std::fmt::Debug for Node<'t> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Token(arg0) => f.debug_tuple("Token").field(&arg0.text).finish(),
            Self::Parens(arg0) => f.debug_tuple("Parens").field(arg0).finish(),
            Self::Braces(arg0) => f.debug_tuple("Braces").field(arg0).finish(),
            Self::Brackets(arg0) => f.debug_tuple("Brackets").field(arg0).finish(),
            Self::Group {
                items,
                ends_with_semicolon,
            } => f
                .debug_struct("Group")
                .field("items", items)
                .field("ends_with_semicolon", ends_with_semicolon)
                .finish(),
            Self::StatementList(arg0) => f.debug_tuple("StatementList").field(arg0).finish(),
        }
    }
}

fn parse_node(tokens: TList) -> TResult<Node> {
    let mut semi = Vec::new();
    let mut group = Vec::new();
    let mut tokens = tokens;
    let mut found_semicolon = false;
    while !tokens.is_empty() {
        match tokens[0].kind {
            TKind::Semicolon => {
                semi.push(Node::Group {
                    items: group,
                    ends_with_semicolon: true,
                });
                group = Vec::new();
                found_semicolon = true;
                tokens = &tokens[1..];
                continue;
            }
            TKind::OpenBrace => {
                let (rest, node) = parse_node(&tokens[1..])?;
                if rest[0].kind != TKind::CloseBrace {
                    return make_error(&tokens[0], TKind::CloseBrace);
                }
                tokens = &rest[1..];
                group.push(Node::Braces(Box::new(node)));
                if group.len() > 1 && matches!(group[group.len() - 2], Node::Parens(_)) {
                    semi.push(Node::Group {
                        items: group,
                        ends_with_semicolon: false,
                    });
                    group = Vec::new();
                }
            }
            TKind::OpenParen => {
                let (rest, node) = parse_node(&tokens[1..])?;
                if rest[0].kind != TKind::CloseParen {
                    return make_error(&tokens[0], TKind::CloseParen);
                }
                tokens = &rest[1..];
                group.push(Node::Parens(Box::new(node)));
            }
            TKind::OpenBracket => {
                let (rest, node) = parse_node(&tokens[1..])?;
                if rest[0].kind != TKind::CloseBracket {
                    return make_error(&tokens[0], TKind::CloseBracket);
                }
                tokens = &rest[1..];
                group.push(Node::Brackets(Box::new(node)));
            }
            TKind::CloseBrace | TKind::CloseParen | TKind::CloseBracket | TKind::EndOfFile => {
                break;
            }
            _ => {
                group.push(Node::Token(&tokens[0]));
                tokens = &tokens[1..];
            }
        }
    }

    if !group.is_empty() {
        semi.push(Node::Group {
            items: group,
            ends_with_semicolon: false,
        });
    }

    if semi.len() == 1 && !found_semicolon {
        let node = semi.pop().unwrap();
        Ok((tokens, node))
    } else {
        Ok((tokens, Node::StatementList(semi)))
    }
}

pub struct Parser {}

impl Parser {
    pub fn new() -> Self {
        Self {}
    }

    fn parse_declaration(&mut self, node: &mut Node) {
        match node {
            &mut Node::Group {
                ref mut items,
                ends_with_semicolon,
            } => {

            },
            _ => {
                panic!("Top level declaration should be a group");
            },
        }
    }

    fn parse_top_level(&mut self, root_node: &mut Node) {
        match root_node {
            &mut Node::StatementList(ref mut list) => {
                for statement in list.iter_mut() {
                    self.parse_declaration(statement);
                }
            },
            _ => {
                panic!("Top level should be a list");
            },
        }
    }

    pub fn parse(&mut self, tokens: TList) {
        let (rest, mut node) = parse_node(tokens).unwrap();
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
    match node {
        Node::Token(token) => print_token(token, out),
        Node::Parens(ref n) => {
            out.push('(');
            print_node(n, out);
            out.push(')');
        }
        Node::Braces(ref n) => {
            out.push_str("\n{\n");
            print_node(n, out);
            out.push_str("\n}");
        }
        Node::Brackets(ref n) => {
            out.push('[');
            print_node(n, out);
            out.push(']');
        }
        Node::Group { ref items, .. } => {
            for n in items {
                print_node(n, out);
            }
        }
        Node::StatementList(ref list) => {
            for n in list {
                print_node(n, out);
                match n {
                    &Node::Group {
                        ends_with_semicolon,
                        ..
                    } => {
                        if !ends_with_semicolon {
                            out.push('\n');
                        } else {
                            out.push_str(";\n");
                        }
                    }
                    _ => {
                        panic!("StatementList should only have groups inside");
                    }
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
