use system_f_omega::typecheck_describe;

#[test]
fn coverage_fixtures() {
    insta::glob!("coverage/*.hs", |path| {
        let source = std::fs::read_to_string(path).unwrap();
        let name = path.file_name().unwrap().to_string_lossy();
        let output = typecheck_describe(&source, &name);
        insta::assert_snapshot!(output);
    });
}
