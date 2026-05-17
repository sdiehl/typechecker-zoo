use refinement::process_test_lines;

#[test]
fn golden_files() {
    insta::glob!("*.fun", |path| {
        let input = std::fs::read_to_string(path).unwrap();
        let output = process_test_lines(&input).join("\n");
        insta::assert_snapshot!(output);
    });
}
