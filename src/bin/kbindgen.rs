use kbindgen::Tokenizer;

fn main() {
    let file_name = std::env::args().nth(1).unwrap();
    let text = std::fs::read(file_name).unwrap();

    let mut tk = Tokenizer::new(text.as_slice().into());
    let tokens = tk.tokenize();

    for t in tokens.iter().take(100) {
        println!("{:?}", t.text);
    }
}
