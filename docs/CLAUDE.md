GOLDEN RULE is that there is non-trivial code in Markdown. Everything should use
mdbook-include-rs to include code that is unit tested and in sync.

You write in technical precise language.
Avoid lists and bullet points.
Do not use em dashes or emojis.
Use full paragraphs.

Whenever you use a code reference to the source code you use mdbook-include-rs
preprocessor so that the code in docs stays in sync with codebase.

mdbook-include-rs supports the following directives:

```rust
#![source_file!("path/to/file.rs")] - Include entire source file
#![function!("path/to/file.rs", function_name)] - Include complete function
#![function_body!("path/to/file.rs", function_name, [optional_dependencies])] - Include just the function body
#![struct!("path/to/file.rs", struct_name)] - Include struct definition
#![enum!("path/to/file.rs", enum_name)] - Include enum definition
#![trait!("path/to/file.rs", trait_name)] - Include trait definition
#![impl!("path/to/file.rs", struct_name)] - Include implementation block
#![trait_impl!("path/to/file.rs", trait_name for struct_name)] - Include trait implementation
```
