//! Module-level compilation that handles multiple functions

use std::collections::HashMap;

use cranelift::prelude::*;
use cranelift_module::{Module as CraneliftModule, FuncId, Linkage};
use cranelift_object::{ObjectBuilder, ObjectModule};
use target_lexicon::Triple;

use crate::core::{CoreModule, CoreTerm};
use super::{erase, closure, codegen};

/// Compile a full module with all its functions
pub fn compile_module_to_executable(module: &CoreModule, output_path: &str) -> Result<(), String> {
    println!("=== Compiling Module ===");
    
    // Create a modified Core representation that includes all functions
    let mut module_core = create_module_core(module)?;
    
    // Type erasure on the whole module
    let erased = erase::erase(&module_core);
    println!("=== Type Erased Module ===");
    println!("{}", erased.pretty());
    
    // Closure conversion
    let program = closure::closure_convert(&erased);
    println!("=== Closure Converted ===");
    for func in &program.functions {
        println!("Function {}: {} -> ...", func.id, func.param);
        println!("  Free vars: {:?}", func.free_vars);
    }
    println!("Main: {}", program.main.pretty());
    
    // Create cranelift module
    let triple = Triple::host();
    let mut settings = cranelift::codegen::settings::builder();
    use cranelift::prelude::Configurable;
    settings.set("opt_level", "none").unwrap();
    let flags = cranelift::codegen::settings::Flags::new(settings);
    
    let isa = cranelift::codegen::isa::lookup(triple.clone())
        .map_err(|e| format!("Error looking up ISA: {}", e))?
        .finish(flags)
        .map_err(|e| format!("Error creating ISA: {}", e))?;
    
    let builder = ObjectBuilder::new(isa, "output", cranelift_module::default_libcall_names())
        .map_err(|e| format!("Error creating object builder: {}", e))?;
    
    let mut obj_module = ObjectModule::new(builder);
    
    // Compile runtime functions
    compile_runtime_functions(&mut obj_module)?;
    
    // Add module functions as globals
    add_module_functions(&mut obj_module, module)?;
    
    // Compile the program
    let mut codegen = codegen::CodeGen::new(obj_module);
    let _main_func = codegen.compile_program(&program)?;
    
    // Finalize
    let module = codegen.finish();
    let object = module.finish();
    let bytes = object.emit()
        .map_err(|e| format!("Error emitting object: {}", e))?;
    
    // Write object file
    let obj_path = format!("{}.o", output_path);
    std::fs::write(&obj_path, bytes)
        .map_err(|e| format!("Error writing object file: {}", e))?;
    
    // Link to executable
    link_executable(&obj_path, output_path)?;
    
    println!("=== Compilation Complete ===");
    println!("Executable written to: {}", output_path);
    
    Ok(())
}

/// Create a single Core term that represents the whole module
fn create_module_core(module: &CoreModule) -> Result<CoreTerm, String> {
    // Find main function
    let main_def = module.term_defs.iter()
        .find(|def| def.name == "main")
        .ok_or("No main function found")?;
    
    // For now, just return main's body
    // A full implementation would create let-bindings for all functions
    Ok(main_def.body.clone())
}

/// Add module functions as callable entities
fn add_module_functions<M: CraneliftModule>(module: &mut M, core_module: &CoreModule) -> Result<(), String> {
    let pointer_type = module.target_config().pointer_type();
    
    for def in &core_module.term_defs {
        if def.name != "main" {
            // Declare the function
            let mut sig = module.make_signature();
            sig.params.push(AbiParam::new(pointer_type)); // closure
            sig.params.push(AbiParam::new(pointer_type)); // argument  
            sig.returns.push(AbiParam::new(pointer_type)); // result
            
            let _func_id = module.declare_function(&def.name, Linkage::Local, &sig)
                .map_err(|e| format!("Failed to declare {}: {}", def.name, e))?;
            
            // The actual compilation happens in the main codegen pass
        }
    }
    
    Ok(())
}

/// Compile runtime functions
fn compile_runtime_functions<M: CraneliftModule>(module: &mut M) -> Result<(), String> {
    let pointer_type = module.target_config().pointer_type();
    
    // make_int
    {
        let mut sig = module.make_signature();
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(pointer_type));
        
        let func_id = module.declare_function("make_int", Linkage::Local, &sig)
            .map_err(|e| format!("Failed to declare make_int: {}", e))?;
        
        let mut ctx = module.make_context();
        ctx.func.signature = sig;
        
        let mut func_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);
        
        let entry = builder.create_block();
        builder.append_block_params_for_function_params(entry);
        builder.switch_to_block(entry);
        builder.seal_block(entry);
        
        let n = builder.block_params(entry)[0];
        let shifted = builder.ins().ishl_imm(n, 3);
        let tagged = builder.ins().bor_imm(shifted, 1);
        builder.ins().return_(&[tagged]);
        
        builder.finalize();
        module.define_function(func_id, &mut ctx)
            .map_err(|e| format!("Failed to define make_int: {}", e))?;
    }
    
    // Stub for make_closure
    {
        let mut sig = module.make_signature();
        sig.params.push(AbiParam::new(pointer_type));
        sig.returns.push(AbiParam::new(pointer_type));
        
        let func_id = module.declare_function("make_closure", Linkage::Local, &sig)
            .map_err(|e| format!("Failed to declare make_closure: {}", e))?;
        
        let mut ctx = module.make_context();
        ctx.func.signature = sig;
        
        let mut func_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);
        
        let entry = builder.create_block();
        builder.append_block_params_for_function_params(entry);
        builder.switch_to_block(entry);
        builder.seal_block(entry);
        
        let code_ptr = builder.block_params(entry)[0];
        builder.ins().return_(&[code_ptr]);
        
        builder.finalize();
        module.define_function(func_id, &mut ctx)
            .map_err(|e| format!("Failed to define make_closure: {}", e))?;
    }
    
    // Simple apply that just calls through
    {
        let mut sig = module.make_signature();
        sig.params.push(AbiParam::new(pointer_type));
        sig.params.push(AbiParam::new(pointer_type));
        sig.returns.push(AbiParam::new(pointer_type));
        
        let func_id = module.declare_function("apply", Linkage::Local, &sig)
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
        
        let call_sig = builder.import_signature(sig);
        let result = builder.ins().call_indirect(call_sig, closure, &[closure, arg]);
        let result_val = builder.inst_results(result)[0];
        builder.ins().return_(&[result_val]);
        
        builder.finalize();
        module.define_function(func_id, &mut ctx)
            .map_err(|e| format!("Failed to define apply: {}", e))?;
    }
    
    // Stub project_env
    {
        let mut sig = module.make_signature();
        sig.params.push(AbiParam::new(pointer_type));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(pointer_type));
        
        let func_id = module.declare_function("project_env", Linkage::Local, &sig)
            .map_err(|e| format!("Failed to declare project_env: {}", e))?;
        
        let mut ctx = module.make_context();
        ctx.func.signature = sig;
        
        let mut func_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);
        
        let entry = builder.create_block();
        builder.append_block_params_for_function_params(entry);
        builder.switch_to_block(entry);
        builder.seal_block(entry);
        
        let zero = builder.ins().iconst(types::I64, 0);
        builder.ins().return_(&[zero]);
        
        builder.finalize();
        module.define_function(func_id, &mut ctx)
            .map_err(|e| format!("Failed to define project_env: {}", e))?;
    }
    
    Ok(())
}

/// Link the object file to create an executable  
fn link_executable(obj_path: &str, output_path: &str) -> Result<(), String> {
    use std::process::Command;
    
    let wrapper_c = r#"
#include <stdio.h>
#include <stdint.h>

extern uint64_t main_compiled();

int64_t extract_int(uint64_t tagged) {
    return (int64_t)(tagged >> 3);
}

int main() {
    uint64_t result = main_compiled();
    printf("%lld\n", extract_int(result));
    return 0;
}
"#;
    
    let wrapper_path = format!("{}_wrapper.c", output_path);
    std::fs::write(&wrapper_path, wrapper_c)
        .map_err(|e| format!("Failed to write wrapper: {}", e))?;
    
    let output = Command::new("cc")
        .args(&[
            "-o", output_path,
            &wrapper_path,
            obj_path,
        ])
        .output()
        .map_err(|e| format!("Failed to run linker: {}", e))?;
    
    if !output.status.success() {
        return Err(format!("Linker failed: {}", String::from_utf8_lossy(&output.stderr)));
    }
    
    let _ = std::fs::remove_file(&wrapper_path);
    
    Ok(())
}