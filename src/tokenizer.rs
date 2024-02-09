use crate::utils::ByteStr;

struct PToken(&'static [u8], TKind);

static PUNCT_LIST: [PToken; 48] = [
    PToken(b">>=", TKind::Punct),
    PToken(b"<<=", TKind::Punct),
    PToken(b"...", TKind::Punct),
    PToken(b"||", TKind::Punct),
    PToken(b"|=", TKind::Punct),
    PToken(b"^=", TKind::Punct),
    PToken(b">>", TKind::Punct),
    PToken(b">=", TKind::Punct),
    PToken(b"==", TKind::Punct),
    PToken(b"<=", TKind::Punct),
    PToken(b"<<", TKind::Punct),
    PToken(b"/=", TKind::Punct),
    PToken(b"->", TKind::Punct),
    PToken(b"-=", TKind::Punct),
    PToken(b"--", TKind::Punct),
    PToken(b"+=", TKind::Punct),
    PToken(b"++", TKind::Punct),
    PToken(b"*=", TKind::Punct),
    PToken(b"&=", TKind::Punct),
    PToken(b"&&", TKind::Punct),
    PToken(b"%=", TKind::Punct),
    PToken(b"##", TKind::Punct),
    PToken(b"!=", TKind::Punct),
    PToken(b"~", TKind::Punct),
    PToken(b"}", TKind::CloseBrace),
    PToken(b"|", TKind::Punct),
    PToken(b"{", TKind::OpenBrace),
    PToken(b"^", TKind::Punct),
    PToken(b"]", TKind::CloseBracket),
    PToken(b"[", TKind::OpenBracket),
    PToken(b"?", TKind::Punct),
    PToken(b">", TKind::Punct),
    PToken(b"=", TKind::Assignment),
    PToken(b"<", TKind::Punct),
    PToken(b";", TKind::Semicolon),
    PToken(b":", TKind::Colon),
    PToken(b"/", TKind::Punct),
    PToken(b".", TKind::Punct),
    PToken(b"-", TKind::Punct),
    PToken(b",", TKind::Comma),
    PToken(b"+", TKind::Punct),
    PToken(b"*", TKind::Punct),
    PToken(b")", TKind::CloseParen),
    PToken(b"(", TKind::OpenParen),
    PToken(b"&", TKind::Punct),
    PToken(b"%", TKind::Punct),
    PToken(b"#", TKind::Punct),
    PToken(b"!", TKind::Punct),
];

static KEYWORD_LIST: [PToken; 5] = [
    PToken(b"__attribute__", TKind::Attribute),
    PToken(b"typedef", TKind::Typedef),
    PToken(b"struct", TKind::Struct),
    PToken(b"enum", TKind::Enum),
    PToken(b"asm", TKind::Asm),
];

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TKind {
    Ident,
    Punct,
    Number,
    String,
    Char,
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
    Struct,
    Typedef,
    Enum,
    Attribute,
    Asm,
}

#[derive(Debug)]
pub struct Token<'a> {
    pub text: ByteStr<'a>,
    pub kind: TKind,
    pub has_space: bool,
    pub file_name: ByteStr<'a>,
    pub line_number: u32,
}

/// Returns a string literal without the enclosing double quotes and
/// the rest of a string (without the closing double quote)
fn slice_string_literal(text: ByteStr) -> (ByteStr, ByteStr) {
    let (_, rest) = text.slice_where(|c| c != b'"');
    let mut prev = 0;
    let (string, rest) = rest.skip_one().slice_where(|c| {
        let not_quote = (c != b'"') || (prev == b'\\');
        prev = c;
        not_quote
    });
    (string, rest.skip_one())
}

fn slice_char_literal(text: ByteStr) -> (ByteStr, ByteStr) {
    let mut prev = 0;
    let (string, rest) = text.skip_one().slice_where(|c| {
        let not_quote = (c != b'\'') || (prev == b'\\');
        prev = c;
        not_quote
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
}

impl<'t> Tokenizer<'t> {
    pub fn new(full_text: ByteStr<'t>) -> Self {
        Self {
            full_text,
            file_name: ByteStr(b""),
            line_number: 0,
            start_of_line: true,
            has_space: false,
        }
    }

    pub fn new_token<'a>(&'a mut self, t: ByteStr<'t>, k: TKind) -> Token<'t> {
        let token = Token {
            text: t,
            kind: k,
            file_name: self.file_name,
            line_number: self.line_number,
            has_space: self.has_space,
        };
        self.has_space = false;
        self.start_of_line = false;
        token
    }

    pub fn tokenize<'a>(&'a mut self) -> Vec<Token<'t>> {
        let mut text = self.full_text;
        let mut tokens = Vec::with_capacity(text.len() / 5);

        'tokens: while !text.is_empty() {
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
                tokens.push(self.new_token(num_str, TKind::Number));
                continue;
            }

            // Parse char literal
            if byte == b'\'' {
                let (string, rest) = slice_char_literal(text);
                text = rest;
                tokens.push(self.new_token(string, TKind::Char));
                continue;
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
                tokens.push(self.new_token(string, TKind::String));
                continue;
            }

            // Parse punctuation
            for token in PUNCT_LIST.iter() {
                if text.starts_with(token.0) {
                    let (p_str, rest) = text.0.split_at(token.0.len());
                    text = rest.into();
                    tokens.push(self.new_token(p_str.into(), token.1));
                    continue 'tokens;
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
            for keyword in KEYWORD_LIST.iter() {
                if ident.0 == keyword.0 {
                    ident_kind = keyword.1;
                    break;
                }
            }
            tokens.push(self.new_token(ident, ident_kind));
        }
        tokens
    }
}

#[cfg(test)]
mod tests {
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
    }

    #[test]
    fn test_number_literal() {
        let t = ByteStr(b".1e-4lf,");
        let (s, r) = super::slice_number_literal(t);
        assert_eq!(s, ByteStr(b".1e-4lf"));
        assert_eq!(r, ByteStr(b","));
    }
}
