use kbindgen::parser::Parser;
use kbindgen::tokenizer::Tokenizer;

fn main() {
    let file_name = std::env::args().nth(1).unwrap();
    let text = std::fs::read(file_name).unwrap();

    let mut tk = Tokenizer::new(text.as_slice().into());
    let tokens = tk.tokenize();

    let mut pa = Parser::new();
    pa.parse(&tokens);
}
