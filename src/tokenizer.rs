use crate::utils::{ByteStr, FastHashMap};

struct PToken(&'static [u8], TKind);

/// This punctuator list is sorted by frequency from the most used to least used
/// Except in cases where the punctuator is a prefix of another punctuator
/// In that case, the longer punctuator comes first
/// This is verified in the `test_punct_list` unit test
const PUNCT_LIST: &[PToken] = &[
    PToken(b")", TKind::CloseParen),
    PToken(b"(", TKind::OpenParen),
    PToken(b";", TKind::Semicolon),
    PToken(b"*=", TKind::Punct),
    PToken(b"*", TKind::Asterisk),
    PToken(b",", TKind::Comma),
    PToken(b"}", TKind::CloseBrace),
    PToken(b"{", TKind::OpenBrace),
    PToken(b":", TKind::Colon),
    PToken(b"->", TKind::Punct),
    PToken(b"&=", TKind::Punct),
    PToken(b"&&", TKind::Punct),
    PToken(b"&", TKind::Punct),
    PToken(b"==", TKind::Punct),
    PToken(b"=", TKind::Assignment),
    PToken(b"!=", TKind::Punct),
    PToken(b"!", TKind::Punct),
    PToken(b"-=", TKind::Punct),
    PToken(b"--", TKind::Punct),
    PToken(b"-", TKind::Punct),
    PToken(b"<<=", TKind::Punct),
    PToken(b"<<", TKind::Punct),
    PToken(b"+=", TKind::Punct),
    PToken(b"++", TKind::Punct),
    PToken(b"+", TKind::Punct),
    PToken(b"]", TKind::CloseBracket),
    PToken(b"[", TKind::OpenBracket),
    PToken(b"?", TKind::Punct),
    PToken(b"||", TKind::Punct),
    PToken(b"...", TKind::Punct),
    PToken(b".", TKind::Punct),
    PToken(b"|=", TKind::Punct),
    PToken(b"|", TKind::Punct),
    PToken(b"~", TKind::Punct),
    PToken(b">>=", TKind::Punct),
    PToken(b">>", TKind::Punct),
    PToken(b"<=", TKind::Punct),
    PToken(b"<", TKind::Punct),
    PToken(b">=", TKind::Punct),
    PToken(b">", TKind::Punct),
    PToken(b"/=", TKind::Punct),
    PToken(b"/", TKind::Punct),
    PToken(b"%=", TKind::Punct),
    PToken(b"%", TKind::Punct),
    PToken(b"^=", TKind::Punct),
    PToken(b"^", TKind::Punct),
    PToken(b"##", TKind::Punct),
    PToken(b"#", TKind::Punct),
];

const KEYWORD_LIST: &[PToken] = &[
    // storage-class-specifier
    PToken(b"typedef", TKind::Typedef),
    PToken(b"extern", TKind::Extern),
    PToken(b"static", TKind::Static),
    PToken(b"_Thread_local", TKind::ThreadLocal),
    PToken(b"auto", TKind::Auto),
    PToken(b"register", TKind::Register),
    // type-specifier
    PToken(b"void", TKind::Void),
    PToken(b"char", TKind::CharType),
    PToken(b"short", TKind::Short),
    PToken(b"int", TKind::Int),
    PToken(b"long", TKind::Long),
    PToken(b"float", TKind::Float),
    PToken(b"double", TKind::Double),
    PToken(b"__signed__", TKind::Signed),
    PToken(b"signed", TKind::Signed),
    PToken(b"unsigned", TKind::Unsigned),
    PToken(b"_Bool", TKind::Bool),
    PToken(b"_Complex", TKind::Complex),
    PToken(b"__int128", TKind::Int128),
    PToken(b"__int128_t", TKind::Int128),
    PToken(b"__uint128_t", TKind::UInt128),
    // struct, union, enum
    PToken(b"struct", TKind::Struct),
    PToken(b"union", TKind::Union),
    PToken(b"enum", TKind::Enum),
    // type-qualifier
    PToken(b"const", TKind::Const),
    PToken(b"restrict", TKind::Restrict),
    PToken(b"volatile", TKind::Volatile),
    PToken(b"_Atomic", TKind::Atomic),
    // function-specifier
    PToken(b"inline", TKind::Inline),
    PToken(b"_Noreturn", TKind::Noreturn),
    // alignment-specifier
    PToken(b"_Alignas", TKind::Alignas),
    // other keywords
    PToken(b"_Static_assert", TKind::StaticAssert),
    PToken(b"__attribute__", TKind::Attribute),
    PToken(b"__typeof__", TKind::Typeof),
    PToken(b"typeof", TKind::Typeof),
    PToken(b"__extension__", TKind::Extension),
    PToken(b"__builtin_va_list", TKind::BuiltinVaList),
];

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TKind {
    Ident,
    Punct,
    Number,
    String,
    Char,
    Asterisk,
    Assignment,
    Comma,
    Colon,
    Semicolon,
    OpenBrace,
    CloseBrace,
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    // storage-class-specifier
    Typedef,
    Extern,
    Static,
    ThreadLocal,
    Auto,
    Register,
    // type-specifier
    Void,
    CharType,
    Short,
    Int,
    Long,
    Float,
    Double,
    Signed,
    Unsigned,
    Bool,
    Complex,
    Int128,
    UInt128,
    // struct, union, enum
    Struct,
    Union,
    Enum,
    // type-qualifier
    Const,
    Restrict,
    Volatile,
    Atomic,
    // function-specifier
    Inline,
    Noreturn,
    // alignment-specifier
    Alignas,
    // other keywords
    StaticAssert,
    Attribute,
    Typeof,
    Extension,
    BuiltinVaList,
    // EOF
    EndOfFile,
}

#[derive(Debug)]
pub struct Token<'a> {
    pub text: ByteStr<'a>,
    pub kind: TKind,
    pub has_space: bool,
    pub start_of_line: bool,
    pub file_name: ByteStr<'a>,
    pub line_number: u32,
}

/// Returns a string literal without the enclosing double quotes and
/// the rest of a string (without the closing double quote)
fn slice_string_literal(text: ByteStr) -> (ByteStr, ByteStr) {
    let (_, rest) = text.slice_where(|c| c != b'"');
    let mut prev = 0;
    let (string, rest) = rest.skip_one().slice_where(|c| {
        if prev == b'\\' {
            prev = 0;
            true
        } else {
            prev = c;
            c != b'"'
        }
    });
    (string, rest.skip_one())
}

fn slice_char_literal(text: ByteStr) -> (ByteStr, ByteStr) {
    let mut prev = 0;
    let (string, rest) = text.skip_one().slice_where(|c| {
        if prev == b'\\' {
            prev = 0;
            true
        } else {
            prev = c;
            c != b'\''
        }
    });
    (string, rest.skip_one())
}

fn slice_number_literal(text: ByteStr) -> (ByteStr, ByteStr) {
    let mut prev = 0;
    let (num_str, rest) = text.slice_where(|c| {
        let is_dot = c == b'.';
        let is_alpha = c.is_ascii_alphanumeric();
        let is_exp = (c == b'+' || c == b'-')
            && (prev == b'e' || prev == b'E' || prev == b'p' || prev == b'P');
        prev = c;
        is_dot || is_alpha || is_exp
    });
    (num_str, rest)
}

pub struct Tokenizer<'t> {
    full_text: ByteStr<'t>,
    file_name: ByteStr<'t>,
    line_number: u32,
    start_of_line: bool,
    has_space: bool,
    keywords: FastHashMap<ByteStr<'t>, TKind>,
}

impl<'t> Tokenizer<'t> {
    pub fn new(full_text: ByteStr<'t>) -> Self {
        let mut tk = Self {
            full_text,
            file_name: ByteStr(b""),
            line_number: 0,
            start_of_line: true,
            has_space: false,
            keywords: FastHashMap::default(),
        };
        for kw in KEYWORD_LIST {
            tk.keywords.insert(ByteStr(kw.0), kw.1);
        }
        tk
    }

    pub fn tokenize<'a>(&'a mut self) -> Vec<Token<'t>> {
        let mut tokens = Vec::with_capacity(self.full_text.len() / 5);
        loop {
            let token = self.next_token();
            if token.kind == TKind::EndOfFile {
                tokens.push(token);
                break;
            }
            tokens.push(token);
        }
        tokens
    }

    fn new_token(&mut self, t: ByteStr<'t>, k: TKind) -> Token<'t> {
        let token = Token {
            text: t,
            kind: k,
            file_name: self.file_name,
            line_number: self.line_number,
            has_space: self.has_space,
            start_of_line: self.start_of_line,
        };
        self.has_space = false;
        self.start_of_line = false;
        token
    }

    pub fn next_token(&mut self) -> Token<'t> {
        let mut text = self.full_text;

        while !text.is_empty() {
            let byte = text[0];

            // Skip whitespace
            if byte.is_ascii_whitespace() {
                if byte == b'\n' {
                    self.line_number += 1;
                    self.start_of_line = true;
                    self.has_space = false;
                } else {
                    self.has_space = true;
                }
                text = text.skip_one();
                continue;
            }

            // Parse preprocessor line
            if self.start_of_line && byte == b'#' {
                // Take the whole line from the text
                let (pp, rest) = text.slice_where(|c| c != b'\n');

                // Parse the line number and file name string
                if pp.starts_with(b"# ") {
                    let pp = pp.skip_one().trim_start();
                    let (num_str, pp) = pp.slice_where(|c| c.is_ascii_digit());
                    if let Ok(num) = num_str.parse() {
                        let (name_str, _) = slice_string_literal(pp);
                        self.line_number = num;
                        self.file_name = name_str;
                    }
                }

                // Skip the newline character
                text = rest.skip_one();
                continue;
            }

            // Parse numbers
            if byte.is_ascii_digit() || (byte == b'.' && text.len() > 1 && text[1].is_ascii_digit())
            {
                let (num_str, rest) = slice_number_literal(text);
                text = rest;

                self.full_text = text;
                return self.new_token(num_str, TKind::Number);
            }

            // Parse char literal
            if byte == b'\'' {
                let (string, rest) = slice_char_literal(text);
                text = rest;

                self.full_text = text;
                return self.new_token(string, TKind::Char);
            }

            // Parse string literal
            if byte == b'"'
                || text.starts_with(b"u8\"")
                || text.starts_with(b"u\"")
                || text.starts_with(b"U\"")
                || text.starts_with(b"L\"")
            {
                let (string, rest) = slice_string_literal(text);
                text = rest;

                self.full_text = text;
                return self.new_token(string, TKind::String);
            }

            // Parse punctuation
            for token in PUNCT_LIST.iter() {
                if text.starts_with(token.0) {
                    let (p_str, rest) = text.0.split_at(token.0.len());
                    text = rest.into();

                    self.full_text = text;
                    return self.new_token(p_str.into(), token.1);
                }
            }

            // Identifiers
            let (ident, rest) = text.slice_where(|c| {
                let is_ws = c.is_ascii_whitespace();
                let is_punct = c.is_ascii_punctuation() && c != b'$' && c != b'_';
                !is_ws && !is_punct
            });
            text = rest;

            if ident.is_empty() {
                panic!(
                    "Found unexpected sequence in the input text at {:?}:{}",
                    self.file_name, self.line_number
                );
            }

            let mut ident_kind = TKind::Ident;
            if let Some(kind) = self.keywords.get(&ident) {
                ident_kind = *kind;
            }

            self.full_text = text;
            return self.new_token(ident, ident_kind);
        }

        self.full_text = text;
        self.new_token(text, TKind::EndOfFile)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::ByteStr;

    #[test]
    fn test_string_literal() {
        let t = ByteStr(b"u8\"abc\"x");
        let (s, r) = super::slice_string_literal(t);
        assert_eq!(s, ByteStr(b"abc"));
        assert_eq!(r, ByteStr(b"x"));
    }

    #[test]
    fn test_char_literal() {
        let t = ByteStr(b"'\\''cv");
        let (s, r) = super::slice_char_literal(t);
        assert_eq!(s, ByteStr(b"\\'"));
        assert_eq!(r, ByteStr(b"cv"));
        let t = ByteStr(b"'\\\\'cv");
        let (s, r) = super::slice_char_literal(t);
        assert_eq!(s, ByteStr(b"\\\\"));
        assert_eq!(r, ByteStr(b"cv"));
    }

    #[test]
    fn test_number_literal() {
        let t = ByteStr(b".1e-4lf,");
        let (s, r) = super::slice_number_literal(t);
        assert_eq!(s, ByteStr(b".1e-4lf"));
        assert_eq!(r, ByteStr(b","));
    }

    #[test]
    fn test_punct_list() {
        for punct in PUNCT_LIST {
            let text = ByteStr(punct.0);
            for token in PUNCT_LIST {
                if text.starts_with(token.0) {
                    assert_eq!(text, ByteStr(token.0));
                    break;
                }
            }
        }
    }
}
