use kbindgen::parser::{self, Parser};
use kbindgen::tokenizer::Tokenizer;

fn main() {
    let file_name = std::env::args().nth(1).unwrap();
    let text = std::fs::read(&file_name).unwrap();

    let mut tk = Tokenizer::new(text.as_slice().into());
    let tokens = tk.tokenize();

    let mut pa = Parser::new();
    if let Err(err) = pa.parse(&tokens) {
        parser::print_error("error", &err);
    }

    let mut queries = String::new();
    queries.push_str(&format!("#include \"{}\"\n", file_name));
    pa.generate_queries(&mut queries);
    std::fs::write("queries.c", queries).unwrap();
}
