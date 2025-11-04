//! Executable generation - creates a complete executable with embedded runtime

use std::process::Command;
use std::str::FromStr;

use cranelift::codegen::isa::lookup;
use cranelift::codegen::settings::{builder, Flags};
use cranelift::prelude::{
    types, AbiParam, Configurable, FunctionBuilder, FunctionBuilderContext, InstBuilder,
};
use cranelift_module::{Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use target_lexicon::Triple;

use super::{closure, compile, erase, runtime};
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
        target_lexicon::Triple::from_str("aarch64-apple-darwin")
            .map_err(|e| format!("Error parsing triple: {}", e))?
    } else {
        Triple::host()
    };

    let mut settings = builder();
    settings
        .set("opt_level", "none")
        .map_err(|e| format!("Error setting opt_level: {}", e))?;
    settings
        .set("is_pic", "true")
        .map_err(|e| format!("Error setting is_pic: {}", e))?;
    if cfg!(target_arch = "aarch64") {
        // Ensure proper alignment for ARM64
        settings
            .set("enable_heap_access_spectre_mitigation", "false")
            .map_err(|e| format!("Error setting heap_access_spectre_mitigation: {}", e))?;
        settings
            .set("use_colocated_libcalls", "false")
            .map_err(|e| format!("Error setting use_colocated_libcalls: {}", e))?;
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
    let mut codegen = compile::CodeGen::new(obj_module, runtime_funcs);
    let _main_func = codegen.compile_program(&program)?;

    // Add entry point that calls our main
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
fn add_entry_point<M: Module>(codegen: &mut compile::CodeGen<M>) -> Result<(), String> {
    // Get the module and create entry point function
    let module = codegen.module_mut();

    // Create signature for entry point - takes no arguments, returns i32 (exit
    // code)
    let mut sig = module.make_signature();
    sig.returns.push(AbiParam::new(types::I32));

    // Declare the entry point function with export linkage
    let entry_func = module
        .declare_function("main", Linkage::Export, &sig)
        .map_err(|e| format!("Failed to declare entry point: {}", e))?;

    // Get reference to our compiled main function
    // main_compiled returns a pointer (tagged unit value)
    let mut main_sig = module.make_signature();
    let pointer_type = module.target_config().pointer_type();
    main_sig.returns.push(AbiParam::new(pointer_type));

    let main_func = module
        .declare_function("main_compiled", Linkage::Import, &main_sig)
        .map_err(|e| format!("Failed to declare main_compiled reference: {}", e))?;

    // Create the function body
    let mut ctx = module.make_context();
    ctx.func.signature = sig;

    {
        let mut func_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);

        // Create entry block
        let block = builder.create_block();
        builder.switch_to_block(block);
        builder.seal_block(block);

        // Call our compiled main function
        let main_ref = module.declare_func_in_func(main_func, builder.func);
        builder.ins().call(main_ref, &[]);

        // Return 0 as exit code
        let zero = builder.ins().iconst(types::I32, 0);
        builder.ins().return_(&[zero]);

        // Finalize
        builder.finalize();
    }

    // Define the function
    module
        .define_function(entry_func, &mut ctx)
        .map_err(|e| format!("Failed to define entry point: {}", e))?;

    Ok(())
}

/// Link the object file to create an executable
fn link_executable(obj_path: &str, output_path: &str) -> Result<(), String> {
    // Get the pre-compiled runtime object path from build.rs
    let runtime_obj_path = env!("RUNTIME_OBJ");

    // Link the generated code with the runtime support
    let output = Command::new("cc")
        .args(["-o", output_path, obj_path, runtime_obj_path])
        .output()
        .map_err(|e| format!("Failed to run linker: {}", e))?;

    if !output.status.success() {
        return Err(format!(
            "Linker failed: {}",
            String::from_utf8_lossy(&output.stderr)
        ));
    }

    Ok(())
}
