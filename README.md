# Prototype kbindgen (WIP)

An attempt to make a small alternative to bindgen which doesn't rely on compiler libraries (such as libclang).

### How to run it

```bash
cargo run --release test.c

# or to test the first stage
gcc -S some.c
gcc -E some.c -o some.prep
cargo run --release some.prep
gcc -S stage1.c
diff -u some.s stage1.s
```

It will output the

### The idea

What I want to do is to make an alternative to bindgen which is very simple and has little or no dependencies. Ideally you would have the kernel source code and a Rust compiler and should be able to build the kernel with Rust enabled.

To accomplish this I am trying to find a way to get all required information about the functions and data types using the public interfaces of a compiler. It should not rely on the private flags and it should work with both gcc and clang, no matter which you are using to build the kernel.

So far I found 2 such flags a `-E` to output the preprocessed source code so we don't need to write a full preprocessor and evaluate all #ifdef statements. The other flag is a combination of `-dM -E` which outputs only macro statements which we can parse to get constants.

The algorithm is as follows:

1. Generate preprocessed source using `-E` flag
2. Parse it using very simple parser which only finds global declarations and completely skips over function bodies as we don't need them
3. Collect all the names for structs and it's fields in a big array
4. Output a new C source file which contains a huge array filed with `sizeof`, `alignof` and so on for all the struct fields
5. Compile this source file using the -S flag to output assembly
6. We read the lines of an assembly file until we find the label for this huge array and then start parsing the numbers to get all the information about the struct
7. Now we only need the values for constants. We can parse them from a file generated with `-dM -E`. At this step we can first do the same as bindgen does currently, but ideally we would need a proper macro expansion pass here to parse all the nested macros.
8. Generate the Rust code. No idea how much work it will take. If it's very hard maybe we will have to merge with bindgen but I hope not, as bindgen is very tightly integrated with libclang so it will take a lot of work to separate them.
