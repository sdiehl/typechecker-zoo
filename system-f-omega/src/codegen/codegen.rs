//! Cranelift code generation - fixed version
//!
//! Compiles closure-converted code to machine code using Cranelift

use cranelift::prelude::*;
use cranelift_module::{FuncId, Linkage, Module};

use super::closure::{Closed, Function, FunctionId, Program};

/// Code generator state
pub struct CodeGen<M: Module> {
    module: M,
    builder_context: FunctionBuilderContext,
    ctx: codegen::Context,
    /// Maps function IDs to Cranelift function IDs
    function_map: std::collections::HashMap<FunctionId, FuncId>,
    /// Runtime function declarations
    runtime_funcs: RuntimeFuncs,
}

/// Runtime function IDs
struct RuntimeFuncs {
    make_closure: FuncId,
    project_env: FuncId,
    apply: FuncId,
    make_int: FuncId,
    #[allow(dead_code)]
    make_bool: FuncId,
}

impl<M: Module> CodeGen<M> {
    /// Finish compilation and return the module
    pub fn finish(self) -> M {
        self.module
    }

    pub fn new(mut module: M) -> Self {
        let pointer_type = module.target_config().pointer_type();

        // Declare runtime functions
        let runtime_funcs = RuntimeFuncs {
            make_closure: declare_make_closure(&mut module, pointer_type),
            project_env: declare_project_env(&mut module, pointer_type),
            apply: declare_apply(&mut module, pointer_type),
            make_int: declare_make_int(&mut module, pointer_type),
            make_bool: declare_make_bool(&mut module, pointer_type),
        };

        let ctx = module.make_context();

        Self {
            module,
            builder_context: FunctionBuilderContext::new(),
            ctx,
            function_map: std::collections::HashMap::new(),
            runtime_funcs,
        }
    }

    /// Compile a program
    pub fn compile_program(&mut self, program: &Program) -> Result<FuncId, String> {
        // First pass: declare all functions
        for func in &program.functions {
            let func_id = self.declare_function(func)?;
            self.function_map.insert(func.id, func_id);
        }

        // Second pass: compile function bodies
        for func in &program.functions {
            self.compile_function(func)?;
        }

        // Compile main expression as a function
        self.compile_main(&program.main)
    }

    /// Declare a function
    fn declare_function(&mut self, func: &Function) -> Result<FuncId, String> {
        let name = format!("func_{}", func.id);
        let func_id = self
            .module
            .declare_function(&name, Linkage::Local, &self.function_signature())
            .map_err(|e| format!("Failed to declare function: {}", e))?;
        Ok(func_id)
    }

    /// Get the standard function signature (closure, arg) -> value
    fn function_signature(&self) -> Signature {
        let mut sig = self.module.make_signature();
        let pointer_type = self.module.target_config().pointer_type();
        sig.params.push(AbiParam::new(pointer_type)); // closure
        sig.params.push(AbiParam::new(pointer_type)); // argument
        sig.returns.push(AbiParam::new(pointer_type)); // result
        sig
    }

    /// Compile a function
    fn compile_function(&mut self, func: &Function) -> Result<(), String> {
        self.ctx.func.signature = self.function_signature();

        // Ensure proper alignment for ARM64
        self.ctx.set_disasm(true);

        {
            let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
            let entry_block = builder.create_block();

            builder.append_block_params_for_function_params(entry_block);
            builder.switch_to_block(entry_block);
            builder.seal_block(entry_block);

            // Set up variable environment
            let mut env = Environment::new();

            // Get parameters
            let closure_val = builder.block_params(entry_block)[0];
            let arg_val = builder.block_params(entry_block)[1];

            env.bind("$closure".to_string(), closure_val);
            env.bind(func.param.clone(), arg_val);

            // Compile body - use helper to avoid borrow issues
            let result = compile_expr(
                &mut self.module,
                &self.runtime_funcs,
                &self.function_map,
                &mut builder,
                &func.body,
                &mut env,
            )?;

            builder.ins().return_(&[result]);
            builder.finalize();
        }

        // Define the function
        let func_id = self.function_map[&func.id];
        self.module
            .define_function(func_id, &mut self.ctx)
            .map_err(|e| format!("Failed to define function: {}", e))?;

        self.module.clear_context(&mut self.ctx);
        Ok(())
    }

    /// Compile the main expression
    fn compile_main(&mut self, main: &Closed) -> Result<FuncId, String> {
        let func_id = self
            .module
            .declare_function("main_compiled", Linkage::Export, &self.main_signature())
            .map_err(|e| format!("Failed to declare main: {}", e))?;

        self.ctx.func.signature = self.main_signature();

        // Ensure proper alignment for ARM64
        self.ctx.set_disasm(true);

        {
            let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
            let entry_block = builder.create_block();

            builder.switch_to_block(entry_block);
            builder.seal_block(entry_block);

            let mut env = Environment::new();
            let result = compile_expr(
                &mut self.module,
                &self.runtime_funcs,
                &self.function_map,
                &mut builder,
                main,
                &mut env,
            )?;

            builder.ins().return_(&[result]);
            builder.finalize();
        }

        self.module
            .define_function(func_id, &mut self.ctx)
            .map_err(|e| format!("Failed to define main: {}", e))?;

        Ok(func_id)
    }

    /// Get the main function signature () -> value
    fn main_signature(&self) -> Signature {
        let mut sig = self.module.make_signature();
        let pointer_type = self.module.target_config().pointer_type();
        sig.returns.push(AbiParam::new(pointer_type));
        sig
    }
}

/// Compile a closed expression (extracted to avoid borrow checker issues)
fn compile_expr<M: Module>(
    module: &mut M,
    runtime_funcs: &RuntimeFuncs,
    function_map: &std::collections::HashMap<FunctionId, FuncId>,
    builder: &mut FunctionBuilder,
    expr: &Closed,
    env: &mut Environment,
) -> Result<cranelift::prelude::Value, String> {
    match expr {
        Closed::Var(name) => env
            .lookup(name)
            .ok_or_else(|| format!("Unbound variable: {}", name)),

        Closed::Proj(closure, idx) => {
            let closure_val =
                compile_expr(module, runtime_funcs, function_map, builder, closure, env)?;
            let idx_val = builder.ins().iconst(types::I64, *idx as i64);
            let project_func = module.declare_func_in_func(runtime_funcs.project_env, builder.func);
            let inst = builder.ins().call(project_func, &[closure_val, idx_val]);
            Ok(builder.inst_results(inst)[0])
        }

        Closed::MakeClosure(func_id, captured) => {
            // Get function pointer
            let cranelift_func_id = function_map[func_id];
            let func_ref = module.declare_func_in_func(cranelift_func_id, builder.func);
            let func_ptr = builder
                .ins()
                .func_addr(module.target_config().pointer_type(), func_ref);

            // Compile captured values
            let mut captured_vals = vec![func_ptr];
            for cap in captured {
                captured_vals.push(compile_expr(
                    module,
                    runtime_funcs,
                    function_map,
                    builder,
                    cap,
                    env,
                )?);
            }

            // Call make_closure
            let make_closure_func =
                module.declare_func_in_func(runtime_funcs.make_closure, builder.func);
            let inst = builder.ins().call(make_closure_func, &captured_vals);
            Ok(builder.inst_results(inst)[0])
        }

        Closed::Call(f, x) => {
            let f_val = compile_expr(module, runtime_funcs, function_map, builder, f, env)?;
            let x_val = compile_expr(module, runtime_funcs, function_map, builder, x, env)?;
            let apply_func = module.declare_func_in_func(runtime_funcs.apply, builder.func);
            let inst = builder.ins().call(apply_func, &[f_val, x_val]);
            Ok(builder.inst_results(inst)[0])
        }

        Closed::Int(n) => {
            let n_val = builder.ins().iconst(types::I64, *n);
            let make_int_func = module.declare_func_in_func(runtime_funcs.make_int, builder.func);
            let inst = builder.ins().call(make_int_func, &[n_val]);
            Ok(builder.inst_results(inst)[0])
        }

        Closed::BinOp(op, l, r) => {
            use crate::core::CoreBinOp;

            // For integer operations, we need to extract the integer values
            let l_compiled = compile_expr(module, runtime_funcs, function_map, builder, l, env)?;
            let r_compiled = compile_expr(module, runtime_funcs, function_map, builder, r, env)?;

            // Extract integer values (shift right by 3 to remove tag)
            let l_int = builder.ins().ushr_imm(l_compiled, 3);
            let r_int = builder.ins().ushr_imm(r_compiled, 3);

            // Perform the operation
            let result_int = match op {
                CoreBinOp::Add => builder.ins().iadd(l_int, r_int),
                CoreBinOp::Sub => builder.ins().isub(l_int, r_int),
                CoreBinOp::Mul => builder.ins().imul(l_int, r_int),
                CoreBinOp::Div => builder.ins().sdiv(l_int, r_int),
                CoreBinOp::Lt => {
                    let cmp = builder.ins().icmp(IntCC::SignedLessThan, l_int, r_int);
                    // Convert boolean to 0/1
                    let zero = builder.ins().iconst(types::I64, 0);
                    let one = builder.ins().iconst(types::I64, 1);
                    builder.ins().select(cmp, one, zero)
                }
                CoreBinOp::Le => {
                    let cmp = builder
                        .ins()
                        .icmp(IntCC::SignedLessThanOrEqual, l_int, r_int);
                    // Convert boolean to 0/1
                    let zero = builder.ins().iconst(types::I64, 0);
                    let one = builder.ins().iconst(types::I64, 1);
                    builder.ins().select(cmp, one, zero)
                }
            };

            // Re-tag the result
            let shifted = builder.ins().ishl_imm(result_int, 3);
            let tagged = builder.ins().bor_imm(shifted, 1); // Integer tag
            Ok(tagged)
        }

        Closed::If(c, t, e) => {
            // Compile condition
            let cond_val = compile_expr(module, runtime_funcs, function_map, builder, c, env)?;

            // Extract boolean value (shift right by 3, then check if non-zero)
            let cond_int = builder.ins().ushr_imm(cond_val, 3);
            let zero = builder.ins().iconst(types::I64, 0);
            let cond_bool = builder.ins().icmp(IntCC::NotEqual, cond_int, zero);

            // Create blocks
            let then_block = builder.create_block();
            let else_block = builder.create_block();
            let merge_block = builder.create_block();

            // Add block parameter to merge block for the result
            builder.append_block_param(merge_block, types::I64);

            // Branch on condition
            builder
                .ins()
                .brif(cond_bool, then_block, &[], else_block, &[]);

            // Compile then branch
            builder.switch_to_block(then_block);
            builder.seal_block(then_block);
            let then_val = compile_expr(module, runtime_funcs, function_map, builder, t, env)?;
            builder.ins().jump(merge_block, &[then_val]);

            // Compile else branch
            builder.switch_to_block(else_block);
            builder.seal_block(else_block);
            let else_val = compile_expr(module, runtime_funcs, function_map, builder, e, env)?;
            builder.ins().jump(merge_block, &[else_val]);

            // Continue at merge block
            builder.switch_to_block(merge_block);
            builder.seal_block(merge_block);

            // Return the phi value
            Ok(builder.block_params(merge_block)[0])
        }
    }
}

/// Variable environment for compilation
struct Environment {
    bindings: Vec<(String, cranelift::prelude::Value)>,
}

impl Environment {
    fn new() -> Self {
        Self {
            bindings: Vec::new(),
        }
    }

    fn bind(&mut self, name: String, value: cranelift::prelude::Value) {
        self.bindings.push((name, value));
    }

    fn lookup(&self, name: &String) -> Option<cranelift::prelude::Value> {
        self.bindings
            .iter()
            .rev()
            .find(|(n, _)| n == name)
            .map(|(_, v)| *v)
    }
}

/// Declare runtime functions

fn declare_make_closure<M: Module>(module: &mut M, pointer_type: Type) -> FuncId {
    let mut sig = module.make_signature();
    sig.params.push(AbiParam::new(pointer_type)); // code pointer
                                                  // Variadic captured values would be added dynamically
    sig.returns.push(AbiParam::new(pointer_type));
    module
        .declare_function("make_closure", Linkage::Import, &sig)
        .unwrap()
}

fn declare_project_env<M: Module>(module: &mut M, pointer_type: Type) -> FuncId {
    let mut sig = module.make_signature();
    sig.params.push(AbiParam::new(pointer_type)); // closure
    sig.params.push(AbiParam::new(types::I64)); // index
    sig.returns.push(AbiParam::new(pointer_type));
    module
        .declare_function("project_env", Linkage::Import, &sig)
        .unwrap()
}

fn declare_apply<M: Module>(module: &mut M, pointer_type: Type) -> FuncId {
    let mut sig = module.make_signature();
    sig.params.push(AbiParam::new(pointer_type)); // closure
    sig.params.push(AbiParam::new(pointer_type)); // argument
    sig.returns.push(AbiParam::new(pointer_type));
    module
        .declare_function("apply", Linkage::Import, &sig)
        .unwrap()
}

fn declare_make_int<M: Module>(module: &mut M, pointer_type: Type) -> FuncId {
    let mut sig = module.make_signature();
    sig.params.push(AbiParam::new(types::I64)); // int value
    sig.returns.push(AbiParam::new(pointer_type));
    module
        .declare_function("make_int", Linkage::Import, &sig)
        .unwrap()
}

fn declare_make_bool<M: Module>(module: &mut M, pointer_type: Type) -> FuncId {
    let mut sig = module.make_signature();
    sig.params.push(AbiParam::new(types::I64)); // bool value (0 or 1)
    sig.returns.push(AbiParam::new(pointer_type));
    module
        .declare_function("make_bool", Linkage::Import, &sig)
        .unwrap()
}
