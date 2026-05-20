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
    let output = ["Type", "Prop"]
        .iter()
        .map(|input| match check_term(input) {
            Ok(result) => format!("{} => {}", input, result),
            Err(e) => format!("{} => ERROR: {}", input, e),
        })
        .collect::<Vec<_>>()
        .join("\n");
    insta::assert_snapshot!(output);
}
