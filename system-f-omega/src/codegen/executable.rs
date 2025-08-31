//! Executable generation - creates a complete executable with embedded runtime

use std::str::FromStr;

use cranelift::prelude::*;
use cranelift_module::{Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use target_lexicon::Triple;

use super::{closure, codegen, erase};
use crate::core::{CoreModule, CoreTerm};

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
        println!("Function {}: {} -> ...", func.id, func.param);
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
    let mut settings = cranelift::codegen::settings::builder();
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
    let flags = cranelift::codegen::settings::Flags::new(settings);

    let isa = cranelift::codegen::isa::lookup(triple.clone())
        .map_err(|e| format!("Error looking up ISA: {}", e))?
        .finish(flags)
        .map_err(|e| format!("Error creating ISA: {}", e))?;

    let builder = ObjectBuilder::new(isa, "fibonacci", cranelift_module::default_libcall_names())
        .map_err(|e| format!("Error creating object builder: {}", e))?;

    let mut obj_module = ObjectModule::new(builder);

    // Compile runtime functions inline
    compile_runtime_functions(&mut obj_module)?;

    // Compile the program
    let mut codegen = codegen::CodeGen::new(obj_module);
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

/// Compile runtime functions directly into the module
fn compile_runtime_functions<M: Module>(module: &mut M) -> Result<(), String> {
    let pointer_type = module.target_config().pointer_type();

    // make_int: just shifts and tags
    {
        let mut sig = module.make_signature();
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(pointer_type));

        let func_id = module
            .declare_function("make_int", Linkage::Local, &sig)
            .map_err(|e| format!("Failed to declare make_int: {}", e))?;

        let mut ctx = module.make_context();
        ctx.func.signature = sig;
        ctx.set_disasm(true);

        let mut func_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);

        let entry = builder.create_block();
        builder.append_block_params_for_function_params(entry);
        builder.switch_to_block(entry);
        builder.seal_block(entry);

        let n = builder.block_params(entry)[0];
        let shifted = builder.ins().ishl_imm(n, 3);
        let tagged = builder.ins().bor_imm(shifted, 1); // Integer tag
        builder.ins().return_(&[tagged]);

        builder.finalize();
        module
            .define_function(func_id, &mut ctx)
            .map_err(|e| format!("Failed to define make_int: {}", e))?;
    }

    // Stub implementations for other runtime functions
    // In a real implementation, these would handle closures properly

    // make_closure - for now just return first arg
    {
        let mut sig = module.make_signature();
        sig.params.push(AbiParam::new(pointer_type)); // code pointer
        sig.returns.push(AbiParam::new(pointer_type));

        let func_id = module
            .declare_function("make_closure", Linkage::Local, &sig)
            .map_err(|e| format!("Failed to declare make_closure: {}", e))?;

        let mut ctx = module.make_context();
        ctx.func.signature = sig;
        ctx.set_disasm(true);

        let mut func_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);

        let entry = builder.create_block();
        builder.append_block_params_for_function_params(entry);
        builder.switch_to_block(entry);
        builder.seal_block(entry);

        let code_ptr = builder.block_params(entry)[0];
        builder.ins().return_(&[code_ptr]);

        builder.finalize();
        module
            .define_function(func_id, &mut ctx)
            .map_err(|e| format!("Failed to define make_closure: {}", e))?;
    }

    // apply - for now just call through the function pointer
    {
        let mut sig = module.make_signature();
        sig.params.push(AbiParam::new(pointer_type)); // closure
        sig.params.push(AbiParam::new(pointer_type)); // argument
        sig.returns.push(AbiParam::new(pointer_type));

        let func_id = module
            .declare_function("apply", Linkage::Local, &sig)
            .map_err(|e| format!("Failed to declare apply: {}", e))?;

        let mut ctx = module.make_context();
        ctx.func.signature = sig.clone();

        let mut func_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);

        let entry = builder.create_block();
        builder.append_block_params_for_function_params(entry);
        builder.switch_to_block(entry);
        builder.seal_block(entry);

        let closure = builder.block_params(entry)[0];
        let arg = builder.block_params(entry)[1];

        // Call the closure as a function pointer
        let call_sig = builder.import_signature(sig);
        let result = builder
            .ins()
            .call_indirect(call_sig, closure, &[closure, arg]);
        let result_val = builder.inst_results(result)[0];
        builder.ins().return_(&[result_val]);

        builder.finalize();
        module
            .define_function(func_id, &mut ctx)
            .map_err(|e| format!("Failed to define apply: {}", e))?;
    }

    // project_env - stub that returns 0
    {
        let mut sig = module.make_signature();
        sig.params.push(AbiParam::new(pointer_type));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(pointer_type));

        let func_id = module
            .declare_function("project_env", Linkage::Local, &sig)
            .map_err(|e| format!("Failed to declare project_env: {}", e))?;

        let mut ctx = module.make_context();
        ctx.func.signature = sig;
        ctx.set_disasm(true);

        let mut func_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);

        let entry = builder.create_block();
        builder.append_block_params_for_function_params(entry);
        builder.switch_to_block(entry);
        builder.seal_block(entry);

        let zero = builder.ins().iconst(types::I64, 0);
        builder.ins().return_(&[zero]);

        builder.finalize();
        module
            .define_function(func_id, &mut ctx)
            .map_err(|e| format!("Failed to define project_env: {}", e))?;
    }

    Ok(())
}

/// Add an entry point that calls main and prints the result
fn add_entry_point<M: Module>(_codegen: &mut codegen::CodeGen<M>) -> Result<(), String> {
    // This would add a _start or main function that:
    // 1. Calls our compiled main
    // 2. Extracts the integer value
    // 3. Prints it
    // For now, we'll rely on external linking
    Ok(())
}

/// Create a single CoreTerm that represents the whole module with let-bindings
#[allow(dead_code)]
fn create_module_term(module: &CoreModule) -> Result<CoreTerm, String> {
    use crate::core::CoreTerm;

    // Find main function
    let main_idx = module
        .term_defs
        .iter()
        .position(|def| def.name == "main")
        .ok_or("No main function found")?;

    // Build nested lambda applications to simulate let-bindings
    let mut result = module.term_defs[main_idx].body.clone();

    // Wrap main body with function definitions using lambda application
    // (λf. main_body) function_def
    for (idx, def) in module.term_defs.iter().enumerate() {
        if idx != main_idx {
            // Create: (λf. result) function_body
            // This simulates: let f = function_body in result
            result = CoreTerm::App {
                func: Box::new(CoreTerm::Lambda {
                    param: def.name.clone(),
                    param_ty: def.ty.clone(),
                    body: Box::new(result),
                }),
                arg: Box::new(def.body.clone()),
            };
        }
    }

    Ok(result)
}

/// Link the object file to create an executable
fn link_executable(obj_path: &str, output_path: &str) -> Result<(), String> {
    use std::process::Command;

    // Create a simple C wrapper that calls our main and prints the result
    let wrapper_c = r#"
#include <stdio.h>
#include <stdint.h>

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
        .args(&["-o", output_path, &wrapper_path, obj_path])
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
