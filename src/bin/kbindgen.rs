use kbindgen::Tokenizer;

fn main() {
    let file_name = "/Volumes/CaseSens/rust-linux/rust/bindings/bindings_generated.rs.preproc";
    let text = std::fs::read(file_name).unwrap();

    let mut tk;
    let mut tokens;
    for i in 0..100 {
        tk = Tokenizer::new(text.as_slice().into());
        tokens = tk.tokenize();
    }

    // for t in tokens.iter().take(100) {
    //     println!("{:?}", t.text);
    // }
}
