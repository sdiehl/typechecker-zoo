//! Compilation driver - orchestrates the compilation pipeline

use cranelift_object::{ObjectBuilder, ObjectModule};
use target_lexicon::Triple;

use super::{closure, codegen, erase};
use crate::core::CoreTerm;

/// Compile a Core term to object code
pub fn compile(term: &CoreTerm, output_path: &str) -> Result<(), String> {
    // Phase 1: Type erasure
    let erased = erase::erase(term);
    println!("=== Type Erased ===");
    println!("{}", erased.pretty());

    // Phase 2: Closure conversion
    let program = closure::closure_convert(&erased);
    println!("=== Closure Converted ===");
    println!("Functions:");
    for func in &program.functions {
        println!("  Function {}: {} -> ...", func.id, func.param);
        println!("    Free vars: {:?}", func.free_vars);
        println!("    Body: {}", func.body.pretty());
    }
    println!("Main: {}", program.main.pretty());

    // Phase 3: Code generation
    let triple = Triple::host();
    let mut settings = cranelift::codegen::settings::builder();
    use cranelift::prelude::Configurable;
    settings.set("opt_level", "none").unwrap(); // No optimization for teaching
    let flags = cranelift::codegen::settings::Flags::new(settings);

    let isa = cranelift::codegen::isa::lookup(triple.clone())
        .map_err(|e| format!("Error looking up ISA: {}", e))?
        .finish(flags)
        .map_err(|e| format!("Error creating ISA: {}", e))?;

    let builder = ObjectBuilder::new(
        isa,
        "system_f_omega_output",
        cranelift_module::default_libcall_names(),
    )
    .map_err(|e| format!("Error creating object builder: {}", e))?;

    let module = ObjectModule::new(builder);
    let mut codegen = codegen::CodeGen::new(module);

    let _main_func = codegen.compile_program(&program)?;

    // Get the module back from the codegen
    let module = codegen.finish();
    let bytes = module
        .finish()
        .emit()
        .map_err(|e| format!("Error emitting object: {}", e))?;

    std::fs::write(output_path, bytes).map_err(|e| format!("Error writing object file: {}", e))?;

    println!("=== Compilation Complete ===");
    println!("Object file written to: {}", output_path);

    Ok(())
}

/// Compile and link to executable (requires external linker)
pub fn compile_to_executable(term: &CoreTerm, output_path: &str) -> Result<(), String> {
    // First compile to object file
    let obj_path = format!("{}.o", output_path);
    compile(term, &obj_path)?;

    // Link with runtime (would need to be implemented)
    println!("Note: Linking not implemented. To create executable:");
    println!("1. Compile runtime.c with runtime functions");
    println!("2. Link {} with runtime.o", obj_path);

    Ok(())
}
