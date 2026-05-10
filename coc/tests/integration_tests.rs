use coc::{check_module_string, check_term, process_term_lines};

#[test]
fn golden_files() {
    insta::glob!("*.coc", |path| {
        let input = std::fs::read_to_string(path).unwrap();
        let filename = path.file_name().unwrap().to_string_lossy().into_owned();
        let output = if filename == "basic.coc" {
            process_term_lines(&input).join("\n")
        } else {
            check_module_string(&input, &filename)
        };
        insta::assert_snapshot!(output);
    });
}

#[test]
fn basic_terms() {
    let tests = [("Type", "Type : Type 1"), ("Prop", "Prop : Type")];
    for (input, expected) in tests {
        match check_term(input) {
            Ok(result) => assert_eq!(result, expected),
            Err(e) => panic!("Failed to check term '{}': {}", input, e),
        }
    }
}
