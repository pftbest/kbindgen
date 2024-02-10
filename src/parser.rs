#![allow(dead_code)]

use crate::tokenizer::{TKind, Token};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TErrorKind {
    UnexpectedToken,
    UnbalancedParens,
}

#[derive(Debug)]
struct TError<'t> {
    tokens: TList<'t>,
    kind: TErrorKind,
}

type TRef<'t> = &'t Token<'t>;
type TList<'t> = &'t [Token<'t>];
type TResult<'t, O> = Result<(TList<'t>, O), TError<'t>>;

fn make_error<O>(tokens: TList, kind: TErrorKind) -> TResult<O> {
    Err(TError { tokens, kind })
}

fn kind(tokens: TList, kind: TKind) -> TResult<TRef> {
    let token = &tokens[0];
    if token.kind != kind {
        return make_error(tokens, TErrorKind::UnexpectedToken);
    }
    Ok((&tokens[1..], token))
}

fn parens<'t, F, O>(tokens: TList<'t>, mut f: F) -> TResult<O>
where
    F: FnMut(TList<'t>) -> TResult<O>,
{
    let (tokens, _) = kind(tokens, TKind::OpenParen)?;
    let (tokens, o2) = f(tokens)?;
    let (tokens, _) = kind(tokens, TKind::CloseParen)?;
    Ok((tokens, o2))
}

fn braces<'t, F, O>(tokens: TList<'t>, mut f: F) -> TResult<O>
where
    F: FnMut(TList<'t>) -> TResult<O>,
{
    let (tokens, _) = kind(tokens, TKind::OpenBrace)?;
    let (tokens, o2) = f(tokens)?;
    let (tokens, _) = kind(tokens, TKind::CloseBrace)?;
    Ok((tokens, o2))
}

fn comma_list<'t, F, O>(tokens: TList<'t>, mut f: F) -> TResult<Vec<O>>
where
    F: FnMut(TList<'t>) -> TResult<O>,
{
    let (tokens, out) = f(tokens)?;
    let mut rest = tokens;
    let mut list = Vec::new();
    list.push(out);
    while let Ok((tokens, _)) = kind(rest, TKind::Comma) {
        if let Ok((tokens, out)) = f(tokens) {
            rest = tokens;
            list.push(out);
        } else {
            rest = tokens;
            break;
        }
    }
    Ok((rest, list))
}

fn many_list<'t, F, O>(tokens: TList<'t>, mut f: F) -> TResult<Vec<O>>
where
    F: FnMut(TList<'t>) -> TResult<O>,
{
    let mut rest = tokens;
    let mut list = Vec::new();
    while let Ok((tokens, out)) = f(rest) {
        rest = tokens;
        list.push(out);
    }
    Ok((rest, list))
}

fn balanced_parens(tokens: TList) -> TResult<TList> {
    if tokens[0].kind != TKind::OpenParen {
        return make_error(tokens, TErrorKind::UnexpectedToken);
    }
    let mut depth = 1;
    for i in 1..tokens.len() {
        let t = &tokens[i];
        if t.kind == TKind::OpenParen {
            depth += 1;
        } else if t.kind == TKind::CloseParen {
            depth -= 1;
            if depth == 0 {
                let (out, rest) = tokens.split_at(i);
                return Ok((&rest[1..], &out[1..]));
            }
        }
    }
    make_error(tokens, TErrorKind::UnbalancedParens)
}

fn parse_attribute_name(tokens: TList) -> TResult<TypeAttribute> {
    let (tokens, name) = kind(tokens, TKind::Ident)?;
    let mut tokens = tokens;
    let mut args: TList = &[];
    if let Ok((rest, out)) = balanced_parens(tokens) {
        tokens = rest;
        args = out;
    }
    Ok((tokens, TypeAttribute { name, args }))
}

fn parse_attribute(tokens: TList) -> TResult<Vec<TypeAttribute>> {
    let (tokens, _) = kind(tokens, TKind::Attribute)?;
    parens(tokens, |tokens| {
        parens(tokens, |tokens| {
            comma_list(tokens, |tokens| parse_attribute_name(tokens))
        })
    })
}

#[derive(Debug)]
struct TypeAttribute<'t> {
    name: TRef<'t>,
    args: TList<'t>,
}

// #[derive(Debug)]
// struct SomeEnumMember<'t> {
//     name: TRef<'t>,
//     value: TList<'t>,
// }

// #[derive(Debug)]
// enum SomeType<'t> {
//     StructName {
//         name: TRef<'t>,
//     },
//     Struct {
//         name: Option<TRef<'t>>,
//         members: Vec<SomeTypeDecl<'t>>,
//     },
//     EnumName {
//         name: TRef<'t>,
//     },
//     Enum {
//         name: Option<TRef<'t>>,
//         members: Vec<SomeEnumMember<'t>>,
//     },
//     Primitive {
//         parts: Vec<TRef<'t>>,
//     },
//     Function {
//         ret: Box<SomeType<'t>>,
//         args: Vec<SomeTypeDecl<'t>>,
//     },
//     Pointer {
//         base: Box<SomeType<'t>>,
//     },
// }

// #[derive(Debug)]
// struct SomeTypeDecl<'t> {
//     name: ByteStr<'t>,
//     attributes: Vec<TypeAttribute<'t>>,
//     decl_type: SomeType<'t>,
// }

// fn parse_struct_member(tokens: TList) -> TResult<SomeTypeDecl> {
//     print_tokens(&tokens[..5]);
//     todo!()
// }

// fn parse_struct(tokens: TList) -> TResult<SomeType> {
//     let (tokens, _) = kind(tokens, TKind::Struct)?;
//     let mut tokens = tokens;
//     let mut name = None;
//     if let Ok((rest, n)) = kind(tokens, TKind::Ident) {
//         name = Some(n);
//         tokens = rest;
//     }
//     let (tokens, members) = braces(tokens, |tokens| {
//         many_list(tokens, |tokens| parse_struct_member(tokens))
//     })?;
//     Ok((tokens, SomeType::Struct { name, members }))
// }

// fn parse_type(tokens: TList) -> TResult<SomeType> {
//     match tokens[0].kind {
//         TKind::Struct => {
//             return parse_struct(tokens);
//         }
//         _ => {
//             todo!();
//         }
//     }
//     todo!()
// }

// fn parse_type_decl(tokens: TList) -> TResult<SomeTypeDecl> {
//     let (tokens, tt) = parse_type(tokens)?;
//     todo!()
// }

#[derive(Debug)]
enum Node<'t> {
    Tokens(TList<'t>),
    Parens(Box<Node<'t>>),
    Braces(Box<Node<'t>>),
    Brackets(Box<Node<'t>>),
    Group(Vec<Node<'t>>),
    Function(Vec<Node<'t>>),
    SemicolonList(Vec<Node<'t>>),
}

fn simple_tokens(tokens: TList) -> TResult<TList> {
    for i in 0..tokens.len() {
        match tokens[i].kind {
            TKind::Semicolon
            | TKind::OpenBrace
            | TKind::CloseBrace
            | TKind::OpenParen
            | TKind::CloseParen
            | TKind::OpenBracket
            | TKind::CloseBracket
            | TKind::EndOfFile => {
                let (start, end) = tokens.split_at(i);
                return Ok((end, start));
            }
            _ => {}
        }
    }
    Ok((&tokens[tokens.len()..], tokens))
}

fn parse_node(tokens: TList) -> TResult<Node> {
    let mut semi = Vec::new();
    let mut group = Vec::new();
    let mut tokens = tokens;
    let mut has_semicolon = false;
    while !tokens.is_empty() {
        let (rest, simple) = simple_tokens(tokens)?;
        if !simple.is_empty() {
            tokens = rest;
            group.push(Node::Tokens(simple));
            continue;
        }
        match tokens[0].kind {
            TKind::Semicolon => {
                //if !group.is_empty() {
                    semi.push(Node::Group(group));
                    group = Vec::new();
                    has_semicolon = true;
                //}
                tokens = &tokens[1..];
                continue;
            }
            TKind::OpenBrace => {
                let (rest, node) = parse_node(&tokens[1..])?;
                if rest[0].kind != TKind::CloseBrace {
                    return make_error(rest, TErrorKind::UnexpectedToken);
                }
                tokens = &rest[1..];
                group.push(Node::Braces(Box::new(node)));
                if group.len() > 1 && matches!(group[group.len() - 2], Node::Parens(_)) {
                    semi.push(Node::Function(group));
                    group = Vec::new();
                }
            }
            TKind::OpenParen => {
                let (rest, node) = parse_node(&tokens[1..])?;
                if rest[0].kind != TKind::CloseParen {
                    return make_error(rest, TErrorKind::UnexpectedToken);
                }
                tokens = &rest[1..];
                group.push(Node::Parens(Box::new(node)));
            }
            TKind::OpenBracket => {
                let (rest, node) = parse_node(&tokens[1..])?;
                if rest[0].kind != TKind::CloseBracket {
                    return make_error(rest, TErrorKind::UnexpectedToken);
                }
                tokens = &rest[1..];
                group.push(Node::Brackets(Box::new(node)));
            }
            TKind::CloseBrace | TKind::CloseParen | TKind::CloseBracket | TKind::EndOfFile => {
                break;
            }
            _ => {
                return make_error(tokens, TErrorKind::UnexpectedToken);
            }
        }
    }

    if !group.is_empty() {
        let node = if group.len() == 1 {
            group.pop().unwrap()
        } else {
            Node::Group(group)
        };
        semi.push(node);
    }

    if semi.len() == 1 && !has_semicolon {
        let node = semi.pop().unwrap();
        Ok((tokens, node))
    } else {
        Ok((tokens, Node::SemicolonList(semi)))
    }
}

pub struct Parser {}

impl Parser {
    pub fn new() -> Self {
        Self {}
    }

    pub fn parse(&mut self, tokens: TList) {
        let (rest, node) = parse_node(tokens).unwrap();
        assert_eq!(rest[0].kind, TKind::EndOfFile);
        //println!("{:#?}", node);
        print_node(&node);
    }
}

fn print_node(node: &Node) {
    match node {
        Node::Tokens(tokens) => print_tokens(tokens, false),
        Node::Parens(ref n) => {
            print!("(");
            print_node(n);
            print!(")");
        },
        Node::Braces(ref n) => {
            //print!("{{...}}")
            println!("\n{{");
            print_node(n);
            print!("\n}}");
        },
        Node::Brackets(ref n) => {
            print!("[");
            print_node(n);
            print!("]");
        },
        Node::Group(ref list) => {
            for n in list {
                print_node(n);
            }
        }
        Node::Function(ref list) => {
            println!();
            for n in list {
                print_node(n);
            }
            println!();
        }
        Node::SemicolonList(ref list) => {
            for n in list {
                print_node(n);
                if matches!(n, Node::Function(_)) {
                    println!();
                } else {
                    println!(";");
                }
            }
        }
    }
}

fn print_tokens(tokens: TList, multiline: bool) {
    if multiline {
        println!("------------------------------------");
    }
    let mut end_line = false;
    for t in tokens {
        let mut s = String::from_utf8_lossy(t.text.0).into_owned();
        if t.kind == TKind::String {
            s = format!("\"{}\"", s);
        } else if t.kind == TKind::Char {
            s = format!("'{}'", s);
        }
        if t.has_space || t.start_of_line {
            print!(" {}", s);
            end_line = true;
        } else {
            print!("{}", s);
            end_line = true;
        }
        if t.kind == TKind::Semicolon || t.kind == TKind::OpenBrace {
            if multiline {
                println!();
            }
            end_line = false;
        }
    }
    if multiline {
        if end_line {
            println!()
        }
        println!("------------------------------------");
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
        let (rest, attrs) = parse_attribute(&tokens).unwrap();
        assert_eq!(attrs.len(), 1);
        assert_eq!(attrs[0].name.text, ByteStr(b"packed"));
        assert_eq!(attrs[0].args.len(), 1);
        assert_eq!(rest[0].kind, TKind::EndOfFile);

        let mut tokenizer = Tokenizer::new(ByteStr(b"__attribute__((packed,aligned(4)))"));
        let tokens = tokenizer.tokenize();
        let (rest, attrs) = parse_attribute(&tokens).unwrap();
        assert_eq!(attrs.len(), 2);
        assert_eq!(attrs[0].name.text, ByteStr(b"packed"));
        assert_eq!(attrs[0].args.len(), 0);
        assert_eq!(attrs[1].name.text, ByteStr(b"aligned"));
        assert_eq!(attrs[1].args.len(), 1);
        assert_eq!(rest[0].kind, TKind::EndOfFile);
    }
}
