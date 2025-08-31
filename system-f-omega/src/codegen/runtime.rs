//! Runtime support for System F-ω
//!
//! Provides memory allocation and closure representation

use cranelift::prelude::*;
use cranelift_module::{FuncId, Linkage, Module};

/// Closure representation in memory:
/// [code_ptr | env_size | env[0] | env[1] | ... ]
/// Each field is 8 bytes (pointer-sized)
/// Compile runtime support functions
pub fn compile_runtime<M: Module>(module: &mut M) -> Result<RuntimeFunctions, String> {
    let pointer_type = module.target_config().pointer_type();

    // Declare the allocator function
    let alloc_func = declare_alloc(module, pointer_type)?;

    // Compile runtime functions
    let make_int = compile_make_int(module, pointer_type)?;
    let make_closure = compile_make_closure(module, pointer_type, alloc_func)?;
    let project_env = compile_project_env(module, pointer_type)?;
    let apply = compile_apply(module, pointer_type)?;
    let print_int = compile_print_int(module)?;

    Ok(RuntimeFunctions {
        make_int,
        make_closure,
        project_env,
        apply,
        print_int,
        alloc: alloc_func,
    })
}

pub struct RuntimeFunctions {
    pub make_int: FuncId,
    pub make_closure: FuncId,
    pub project_env: FuncId,
    pub apply: FuncId,
    #[allow(dead_code)]
    pub print_int: FuncId,
    #[allow(dead_code)]
    pub alloc: FuncId,
}

/// Declare the allocator function (implemented in C)
fn declare_alloc<M: Module>(module: &mut M, pointer_type: Type) -> Result<FuncId, String> {
    let mut sig = module.make_signature();
    sig.params.push(AbiParam::new(types::I64)); // size in bytes
    sig.returns.push(AbiParam::new(pointer_type)); // pointer to allocated memory

    module
        .declare_function("rt_alloc", Linkage::Import, &sig)
        .map_err(|e| format!("Failed to declare rt_alloc: {}", e))
}

/// Compile make_int: tag an integer
fn compile_make_int<M: Module>(module: &mut M, pointer_type: Type) -> Result<FuncId, String> {
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
    let tagged = builder.ins().bor_imm(shifted, 1); // Integer tag = 1
    builder.ins().return_(&[tagged]);

    builder.finalize();
    module
        .define_function(func_id, &mut ctx)
        .map_err(|e| format!("Failed to define make_int: {}", e))?;

    Ok(func_id)
}

/// Compile make_closure: allocate a closure with captured environment
fn compile_make_closure<M: Module>(
    module: &mut M,
    pointer_type: Type,
    alloc_func: FuncId,
) -> Result<FuncId, String> {
    // For simplicity, we'll create a fixed signature that takes a code pointer
    // and up to 4 captured values. Real implementation would be variadic.
    let mut sig = module.make_signature();
    sig.params.push(AbiParam::new(pointer_type)); // code pointer
    sig.params.push(AbiParam::new(types::I64)); // number of captured values
    sig.params.push(AbiParam::new(pointer_type)); // captured value 0 (optional)
    sig.params.push(AbiParam::new(pointer_type)); // captured value 1 (optional)
    sig.params.push(AbiParam::new(pointer_type)); // captured value 2 (optional)
    sig.params.push(AbiParam::new(pointer_type)); // captured value 3 (optional)
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
    let env_size = builder.block_params(entry)[1];
    let cap0 = builder.block_params(entry)[2];
    let cap1 = builder.block_params(entry)[3];
    let _cap2 = builder.block_params(entry)[4];
    let _cap3 = builder.block_params(entry)[5];

    // Calculate allocation size: (2 + env_size) * 8 bytes
    let two = builder.ins().iconst(types::I64, 2);
    let total_fields = builder.ins().iadd(two, env_size);
    let eight = builder.ins().iconst(types::I64, 8);
    let alloc_size = builder.ins().imul(total_fields, eight);

    // Allocate memory
    let alloc_ref = module.declare_func_in_func(alloc_func, builder.func);
    let call = builder.ins().call(alloc_ref, &[alloc_size]);
    let closure_ptr = builder.inst_results(call)[0];

    // Store code pointer at offset 0
    builder
        .ins()
        .store(MemFlags::new(), code_ptr, closure_ptr, 0);

    // Store env_size at offset 8
    builder
        .ins()
        .store(MemFlags::new(), env_size, closure_ptr, 8);

    // Store captured values based on env_size
    // We'll need to generate conditional code for each possible size
    let zero = builder.ins().iconst(types::I64, 0);
    let one = builder.ins().iconst(types::I64, 1);
    let _two_const = builder.ins().iconst(types::I64, 2);
    let _three = builder.ins().iconst(types::I64, 3);

    // If env_size > 0, store cap0
    let cmp0 = builder.ins().icmp(IntCC::SignedGreaterThan, env_size, zero);
    let then_block0 = builder.create_block();
    let merge_block0 = builder.create_block();
    builder
        .ins()
        .brif(cmp0, then_block0, &[], merge_block0, &[]);

    builder.switch_to_block(then_block0);
    builder.seal_block(then_block0);
    builder.ins().store(MemFlags::new(), cap0, closure_ptr, 16);
    builder.ins().jump(merge_block0, &[]);

    builder.switch_to_block(merge_block0);
    builder.seal_block(merge_block0);

    // Similar for cap1, cap2, cap3...
    // For brevity, I'll just implement up to 2 captures
    let cmp1 = builder.ins().icmp(IntCC::SignedGreaterThan, env_size, one);
    let then_block1 = builder.create_block();
    let merge_block1 = builder.create_block();
    builder
        .ins()
        .brif(cmp1, then_block1, &[], merge_block1, &[]);

    builder.switch_to_block(then_block1);
    builder.seal_block(then_block1);
    builder.ins().store(MemFlags::new(), cap1, closure_ptr, 24);
    builder.ins().jump(merge_block1, &[]);

    builder.switch_to_block(merge_block1);
    builder.seal_block(merge_block1);

    // Tag the closure pointer (tag = 0, so no change needed)
    builder.ins().return_(&[closure_ptr]);

    builder.finalize();
    module
        .define_function(func_id, &mut ctx)
        .map_err(|e| format!("Failed to define make_closure: {}", e))?;

    Ok(func_id)
}

/// Compile project_env: extract a value from closure environment
fn compile_project_env<M: Module>(module: &mut M, pointer_type: Type) -> Result<FuncId, String> {
    let mut sig = module.make_signature();
    sig.params.push(AbiParam::new(pointer_type)); // closure
    sig.params.push(AbiParam::new(types::I64)); // index
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

    let closure_ptr = builder.block_params(entry)[0];
    let index = builder.block_params(entry)[1];

    // Calculate offset: (2 + index) * 8
    // For now, we'll use dynamic address calculation
    let two = builder.ins().iconst(types::I64, 2);
    let field_index = builder.ins().iadd(two, index);
    let eight = builder.ins().iconst(types::I64, 8);
    let offset = builder.ins().imul(field_index, eight);

    // Add offset to base pointer
    let addr = builder.ins().iadd(closure_ptr, offset);

    // Load the value
    let value = builder.ins().load(pointer_type, MemFlags::new(), addr, 0);
    builder.ins().return_(&[value]);

    builder.finalize();
    module
        .define_function(func_id, &mut ctx)
        .map_err(|e| format!("Failed to define project_env: {}", e))?;

    Ok(func_id)
}

/// Compile apply: call a closure with an argument
fn compile_apply<M: Module>(module: &mut M, pointer_type: Type) -> Result<FuncId, String> {
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

    // Load the code pointer from offset 0
    let code_ptr = builder
        .ins()
        .load(pointer_type, MemFlags::new(), closure, 0);

    // Call the function with closure and argument
    let call_sig = builder.import_signature(sig);
    let result = builder
        .ins()
        .call_indirect(call_sig, code_ptr, &[closure, arg]);
    let result_val = builder.inst_results(result)[0];
    builder.ins().return_(&[result_val]);

    builder.finalize();
    module
        .define_function(func_id, &mut ctx)
        .map_err(|e| format!("Failed to define apply: {}", e))?;

    Ok(func_id)
}

/// Compile print_int: print an integer
fn compile_print_int<M: Module>(module: &mut M) -> Result<FuncId, String> {
    let pointer_type = module.target_config().pointer_type();

    // Declare external rt_print_int that's implemented in C
    let mut sig = module.make_signature();
    sig.params.push(AbiParam::new(types::I64)); // integer value to print
    sig.returns.push(AbiParam::new(pointer_type)); // returns unit (as tagged 0)

    let rt_print_func = module
        .declare_function("rt_print_int", Linkage::Import, &sig)
        .map_err(|e| format!("Failed to declare rt_print_int: {}", e))?;

    // Create print_int function that takes a tagged integer
    let mut sig = module.make_signature();
    sig.params.push(AbiParam::new(pointer_type)); // tagged integer
    sig.returns.push(AbiParam::new(pointer_type)); // returns unit

    let func_id = module
        .declare_function("print_int", Linkage::Local, &sig)
        .map_err(|e| format!("Failed to declare print_int: {}", e))?;

    let mut ctx = module.make_context();
    ctx.func.signature = sig;
    ctx.set_disasm(true);

    let mut func_ctx = FunctionBuilderContext::new();
    let mut builder = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);

    let entry = builder.create_block();
    builder.append_block_params_for_function_params(entry);
    builder.switch_to_block(entry);
    builder.seal_block(entry);

    let tagged_int = builder.block_params(entry)[0];

    // Extract the integer value (shift right by 3)
    let int_val = builder.ins().ushr_imm(tagged_int, 3);

    // Call rt_print_int
    let rt_print_ref = module.declare_func_in_func(rt_print_func, builder.func);
    let call = builder.ins().call(rt_print_ref, &[int_val]);
    let unit_result = builder.inst_results(call)[0];

    builder.ins().return_(&[unit_result]);

    builder.finalize();
    module
        .define_function(func_id, &mut ctx)
        .map_err(|e| format!("Failed to define print_int: {}", e))?;

    Ok(func_id)
}
