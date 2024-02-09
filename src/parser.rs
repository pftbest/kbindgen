use crate::{
    tokenizer::{TKind, Token},
    utils::ByteStr,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TErrorKind {
    Incomplete,
    UnexpectedToken,
}

#[allow(dead_code)]
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
    if tokens.is_empty() {
        return make_error(tokens, TErrorKind::Incomplete);
    }
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
        let (tokens, out) = f(tokens)?;
        rest = tokens;
        list.push(out);
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
    if tokens.is_empty() {
        return make_error(tokens, TErrorKind::Incomplete);
    }
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
                return Ok((rest, &out[1..]));
            }
        }
    }
    make_error(tokens, TErrorKind::Incomplete)
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

#[derive(Debug)]
struct SomeEnumMember<'t> {
    name: TRef<'t>,
    value: TList<'t>,
}

#[derive(Debug)]
enum SomeType<'t> {
    StructName {
        name: TRef<'t>,
    },
    Struct {
        name: Option<TRef<'t>>,
        members: Vec<SomeTypeDecl<'t>>,
    },
    EnumName {
        name: TRef<'t>,
    },
    Enum {
        name: Option<TRef<'t>>,
        members: Vec<SomeEnumMember<'t>>,
    },
    Primitive {
        parts: Vec<TRef<'t>>,
    },
    Function {
        ret: Box<SomeType<'t>>,
        args: Vec<SomeTypeDecl<'t>>,
    },
    Pointer {
        base: Box<SomeType<'t>>,
    },
}

#[derive(Debug)]
struct SomeTypeDecl<'t> {
    name: ByteStr<'t>,
    attributes: Vec<TypeAttribute<'t>>,
    decl_type: SomeType<'t>,
}

fn parse_struct_member(tokens: TList) -> TResult<SomeTypeDecl> {
    todo!()
}

fn parse_struct(tokens: TList) -> TResult<SomeType> {
    let (tokens, _) = kind(tokens, TKind::Struct)?;
    let mut tokens = tokens;
    let mut name = None;
    if let Ok((rest, n)) = kind(tokens, TKind::Ident) {
        name = Some(n);
        tokens = rest;
    }
    let (tokens, members) = braces(tokens, |tokens| {
        many_list(tokens, |tokens| parse_struct_member(tokens))
    })?;
    Ok((tokens, SomeType::Struct { name, members }))
}

fn parse_type(tokens: TList) -> TResult<SomeType> {
    if tokens.is_empty() {
        return make_error(tokens, TErrorKind::Incomplete);
    }
    match tokens[0].kind {
        TKind::Struct => {
            return parse_struct(tokens);
        }
        _ => {
            todo!();
        }
    }

    todo!()
}

fn parse_type_decl(tokens: TList) -> TResult<SomeTypeDecl> {
    let (tokens, tt) = parse_type(tokens)?;
    todo!()
}

/*
#[derive(Debug)]
struct TypeStructMember<'t> {
    name: TRef<'t>,
}

#[derive(Debug)]
struct TypeStruct<'t> {
    name: Option<TRef<'t>>,
    members: Vec<TypeStructMember<'t>>,
}
*/

pub struct Parser {}

impl Parser {
    pub fn new() -> Self {
        Self {}
    }

    pub fn parse(&mut self, tokens: TList) {
        let mut tokens = tokens;
        while tokens.len() > 1 {
            if let Ok((rest, out)) = parse_type(tokens) {
                println!("{:?}", out);
                tokens = rest;
            } else {
                println!("Skipping {:?}", tokens[0].text);
                tokens = &tokens[1..];
                std::process::exit(1);
            }
        }
    }
}

fn print_tokens(tokens: TList) {
    let mut end_line = false;
    for t in tokens {
        let mut s = String::from_utf8_lossy(t.text.0).into_owned();
        if t.kind == TKind::String {
            s = format!("\"{}\"", s);
        }
        if t.has_space {
            print!(" {}", s);
            end_line = true;
        } else {
            print!("{}", s);
            end_line = true;
        }
        if t.kind == TKind::Semicolon || t.kind == TKind::OpenBrace {
            println!();
            end_line = false;
        }
    }
    if end_line {
        println!()
    }
}
