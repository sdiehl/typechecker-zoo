use std::env;
use std::path::PathBuf;
use std::process::Command;

fn main() {
    // Process LALRPOP grammar
    if let Err(e) = lalrpop::process_root() {
        eprintln!("Failed to process lalrpop grammar: {}", e);
        std::process::exit(1);
    }

    // Build runtime support library
    build_runtime_support();
}

fn build_runtime_support() {
    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
    let runtime_src = "src/codegen/runtime_support.rs";
    let runtime_obj = out_dir.join("runtime_support.o");

    println!("cargo:rerun-if-changed={}", runtime_src);

    // Compile Rust runtime support
    let output = Command::new("rustc")
        .args(&[
            "--crate-type=staticlib",
            "--emit=obj",
            "-C",
            "opt-level=2",
            "-C",
            "panic=abort",
            "-C",
            "no-redzone=yes",
            "-o",
        ])
        .arg(&runtime_obj)
        .arg(runtime_src)
        .output()
        .expect("Failed to run rustc");

    if !output.status.success() {
        panic!(
            "Failed to compile runtime support: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    // Set environment variable for the runtime object location
    println!("cargo:rustc-env=RUNTIME_OBJ={}", runtime_obj.display());
}
