//! Executable generation - creates a complete executable with embedded runtime

use std::str::FromStr;

use cranelift::codegen::isa::lookup;
use cranelift::codegen::settings::{builder, Flags};
use cranelift_module::Module;
use cranelift_object::{ObjectBuilder, ObjectModule};
use target_lexicon::Triple;

use super::{closure, cranelift_gen, erase, runtime};
use crate::core::CoreModule;

/// Compile a module to a standalone executable
pub fn compile_executable(module: &CoreModule, output_path: &str) -> Result<(), String> {
    println!("=== Compiling Module ===");

    // Find main function
    let main_def = module
        .term_defs
        .iter()
        .find(|def| def.name == "main")
        .ok_or("No main function found")?;

    // Type erase all functions
    let mut erased_functions = Vec::new();
    for def in &module.term_defs {
        if def.name != "main" {
            let erased = erase::erase(&def.body);
            erased_functions.push((def.name.as_str(), erased));
        }
    }

    // Type erase main
    let erased_main = erase::erase(&main_def.body);

    println!("=== Type Erased Functions ===");
    for (name, body) in &erased_functions {
        println!("{}: {}", name, body.pretty());
    }
    println!("main: {}", erased_main.pretty());

    // Closure conversion with module context
    let program = closure::closure_convert_module(
        erased_functions.iter().map(|(n, b)| (*n, b)).collect(),
        &erased_main,
    );

    println!("=== Closure Converted ===");
    for func in &program.functions {
        println!(
            "Function {}: {} -> {}",
            func.id,
            func.param,
            func.body.pretty()
        );
        println!("  Free vars: {:?}", func.free_vars);
    }
    println!("Main: {}", program.main.pretty());

    // Create cranelift module
    let triple = if cfg!(target_arch = "aarch64") && cfg!(target_os = "macos") {
        // Use the proper ARM64 macOS triple
        target_lexicon::Triple::from_str("aarch64-apple-darwin").unwrap()
    } else {
        Triple::host()
    };
    let mut settings = builder();
    use cranelift::prelude::Configurable;
    settings.set("opt_level", "none").unwrap();
    settings.set("is_pic", "true").unwrap();
    if cfg!(target_arch = "aarch64") {
        // Ensure proper alignment for ARM64
        settings
            .set("enable_heap_access_spectre_mitigation", "false")
            .unwrap();
        settings.set("use_colocated_libcalls", "false").unwrap();
    }
    let flags = Flags::new(settings);

    let isa = lookup(triple.clone())
        .map_err(|e| format!("Error looking up ISA: {}", e))?
        .finish(flags)
        .map_err(|e| format!("Error creating ISA: {}", e))?;

    let builder = ObjectBuilder::new(
        isa,
        "system_f_omega",
        cranelift_module::default_libcall_names(),
    )
    .map_err(|e| format!("Error creating object builder: {}", e))?;

    let mut obj_module = ObjectModule::new(builder);

    // Compile runtime functions
    let runtime_funcs = runtime::compile_runtime(&mut obj_module)?;

    // Compile the program
    let mut codegen = cranelift_gen::CodeGen::new(obj_module, runtime_funcs);
    let _main_func = codegen.compile_program(&program)?;

    // Add a simple entry point that calls our main and prints result
    add_entry_point(&mut codegen)?;

    // Finalize
    let module = codegen.finish();
    let object = module.finish();
    let bytes = object
        .emit()
        .map_err(|e| format!("Error emitting object: {}", e))?;

    // Write object file
    let obj_path = format!("{}.o", output_path);
    std::fs::write(&obj_path, bytes).map_err(|e| format!("Error writing object file: {}", e))?;

    // Link to executable using system linker
    link_executable(&obj_path, output_path)?;

    println!("=== Compilation Complete ===");
    println!("Executable written to: {}", output_path);

    Ok(())
}

/// Add an entry point that calls main and prints the result
fn add_entry_point<M: Module>(_codegen: &mut cranelift_gen::CodeGen<M>) -> Result<(), String> {
    // This would add a _start or main function that:
    // 1. Calls our compiled main
    // 2. Extracts the integer value
    // 3. Prints it
    // For now, we'll rely on external linking
    Ok(())
}

/// Link the object file to create an executable
fn link_executable(obj_path: &str, output_path: &str) -> Result<(), String> {
    use std::process::Command;

    // Create a simple C wrapper that calls our main and prints the result
    let wrapper_c = r#"
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

// Simple bump allocator for runtime
static uint8_t heap[1024 * 1024]; // 1MB heap
static size_t heap_ptr = 0;

// Runtime allocator function
void* rt_alloc(size_t size) {
    // Align to 8 bytes
    size = (size + 7) & ~7;
    
    if (heap_ptr + size > sizeof(heap)) {
        fprintf(stderr, "Out of memory!\n");
        exit(1);
    }
    
    void* result = &heap[heap_ptr];
    heap_ptr += size;
    return result;
}

// Our compiled main function (renamed to avoid conflict)
extern uint64_t main_compiled();

// Extract integer value from tagged pointer
int64_t extract_int(uint64_t tagged) {
    return (int64_t)(tagged >> 3);
}

int main() {
    uint64_t result = main_compiled();
    printf("%lld\n", extract_int(result));
    return 0;
}
"#;

    // Write wrapper
    let wrapper_path = format!("{}_wrapper.c", output_path);
    std::fs::write(&wrapper_path, wrapper_c)
        .map_err(|e| format!("Failed to write wrapper: {}", e))?;

    // Compile and link
    let output = Command::new("cc")
        .args(["-o", output_path, &wrapper_path, obj_path])
        .output()
        .map_err(|e| format!("Failed to run linker: {}", e))?;

    if !output.status.success() {
        return Err(format!(
            "Linker failed: {}",
            String::from_utf8_lossy(&output.stderr)
        ));
    }

    // Clean up wrapper
    let _ = std::fs::remove_file(&wrapper_path);

    Ok(())
}
