# Code Generation

This section is optional, but we'd like to explore a little bit how you we can go from high-level lambda calculus (i.e. System F) all the way down to executable machine code requires several transformations that preserve the semantics of our programs while adapting them to the realities of modern CPU architectures.

We're going to build a minimalist generation pipeline that transforms our typed functional programs into imperative machine code through three major phases:

* **Type erasure** First we removes the type information that guided our type checking,
* **Closure conversion** Second makes the implicit environment capture of nested functions explicit
* **Code generation** Finally our code generation framework (in this case Cranelift) generates optimized machine code for the target architecture from our closure converted code.

## Choosing a Code Generation Backend

Before diving into our implementation, we should understand why we chose Cranelift as our code generation backend. When implementing a compiler, you face a fundamental choice about how to generate executable code, each with distinct tradeoffs.

**Bespoke Backend**: The most educational approach involves directly emitting assembly instructions for your target architecture. This gives you complete control and deep understanding of the machine, but requires implementing register allocation, instruction selection, and optimization passes from scratch. For production compilers targeting multiple architectures, this quickly becomes intractable. You would need to understand the intricacies of x86-64, ARM64, RISC-V, and other instruction sets, along with their calling conventions and performance characteristics. This can be a non-trivial project.

**Virtual Machine Approach**: Many languages choose to compile to bytecode for a custom virtual machine. Languages like Python, Ruby, and early Java implementations take this route. This provides excellent portability and simplifies the compiler, but sacrifices performance due to interpretation overhead. Even with just-in-time compilation, the VM approach typically cannot match native code performance for compute-intensive tasks.

**LibJIT and Lightweight JITs**: Libraries like LibJIT provide a middle ground, offering a simple API for generating machine code without building a full compiler backend. These work well for domain-specific languages and embedded scripting, but typically lack sophisticated optimizations and broad architecture support. They excel at simplicity but plateau quickly in terms of performance.

**LLVM**: The dominant choice for production compilers, LLVM provides industrial-strength optimization passes and supports virtually every architecture. Rust, Swift, and Clang all use LLVM. However, LLVM's comprehensiveness comes with significant costs: massive binary sizes (hundreds of megabytes), slow compilation times, and a complex C++ API that requires deep expertise. LLVM's optimization passes, while powerful, can take longer than the rest of your compiler combined.

**MLIR**: Multi-Level Intermediate Representation is like a higher-level LLVM, which extends LLVM with better support for domain-specific optimizations and heterogeneous computing (think GPUs). While powerful for machine learning compilers and specialized domains, MLIR adds even more complexity than LLVM and is still fairly early and undocumented (although I've tired [somewhat to remedy that](https://www.stephendiehl.com/posts/mlir_introduction/)).

**Cranelift**: Originally designed for WebAssembly, Cranelift occupies a sweet spot for language experimentation. It generates good quality code quickly, has a clean Rust API, produces small binaries, and supports the major architectures (x86-64, ARM64). While it cannot match LLVM's peak optimization quality, Cranelift compiles code an order of magnitude faster. For a teaching compiler where iteration speed and code clarity matter more than squeezing out the last 10% of performance, Cranelift is awesome.

We're going to use Cranelift, because it's simple and uses the Rust build system with no external dependencies. It also has a clean API and compiles quickly, making it ideal for experimentation and learning. But our approach would work  with any of these backends ... only the final code generation phase would change if you choose a different backend.

## Type Erasure

System F-ω's type system serves its purpose during type checking, ensuring program correctness and enabling powerful abstractions. But as we discussed before types exist purely for compile-time verification and carry no computational content at runtime. The type erasure phase strips away all type information, leaving only the essential computational structure.

```rust
#![enum!("system-f-omega/src/codegen/erase.rs", Erased)]
```

The erased representation captures the essence of computation without types. Variables become simple names, lambda abstractions lose their type annotations, and applications remain as the fundamental operation of function invocation. Integer literals and binary operations pass through unchanged, as they represent actual runtime computations. Critically, type abstractions and type applications vanish entirely during erasure, as they exist solely to guide type checking.

```rust
#![function!("system-f-omega/src/codegen/erase.rs", erase)]
```

The erasure algorithm traverses the typed abstract syntax tree and systematically removes all type information. Lambda abstractions lose their parameter type annotations, becoming untyped functions that operate solely on runtime values. Type abstractions disappear entirely, with only their bodies remaining, since type parameters have no runtime representation. Type applications similarly vanish, as they merely instantiate type variables that no longer exist after erasure.

This phase demonstrates a fundamental principle of type systems: types are a compile-time discipline that ensures program correctness without imposing runtime overhead. The erased program retains exactly the computational content of the original while shedding the scaffolding that ensured its correctness.

## Closure Conversion

Functional languages allow functions to be defined within other functions, creating nested scopes where inner functions can reference variables from enclosing scopes. This natural programming style poses a challenge for compilation to machine code, where functions are typically compiled to fixed addresses with no implicit access to their defining environment.

Consider this simple example of a function that creates an adder:

```haskell
makeAdder :: Int -> (Int -> Int);
makeAdder x = λy. x + y;

add5 :: Int -> Int;
add5 = makeAdder 5;
```

The inner lambda `λy. x + y` references the variable `x` from its enclosing scope. When `makeAdder` returns this function, the value of `x` must somehow be preserved so that when `add5` is later called with an argument, it still has access to the value `5` that was passed to `makeAdder`.

In traditional assembly language or C, functions are just code addresses with no associated data. A function like `add` would be compiled to a fixed location in memory:

```
add_function:
    ; expects two arguments in registers
    ; adds them and returns result
```

But our `add5` function needs to remember that `x = 5`. This is where closure conversion comes in.

Closure conversion solves this problem by transforming every function into a pair of a code pointer and an environment containing captured values. The transformation makes environment capture explicit by:

1. Identifying free variables in each function (variables used but not defined locally)
2. Creating an environment structure to hold these captured values
3. Rewriting functions to take an extra parameter (the closure) and extract captured values from it
4. Transforming function calls to pass both the closure and the regular argument

After closure conversion, our example becomes something like:

```
// Original: λy. x + y where x is free
// Converted: λ(closure, y). project(closure, 0) + y

// Creating add5:
// 1. Allocate closure: [code_ptr=add_function, env=[5]]
// 2. Return this closure

// Calling add5(3):
// 1. Extract code pointer from closure
// 2. Call code_ptr(closure, 3)
// 3. Inside function: project(closure, 0) gives us 5
// 4. Return 5 + 3 = 8
```

This transformation turns the implicit variable capture of nested functions into explicit data structures that can be allocated and manipulated at runtime.

```rust
#![enum!("system-f-omega/src/codegen/closure.rs", Closed)]
```

The closed representation introduces several new constructs that make environment manipulation explicit. The `MakeClosure` construct creates a closure by pairing a function identifier with its captured environment. The `Proj` construct extracts values from a closure's environment using positional indices. The `Call` construct invokes a closure by passing both the closure itself and the argument.

```rust
#![struct!("system-f-omega/src/codegen/closure.rs", Function)]
```

After closure conversion, each function exists as a separate top-level definition with an explicit parameter for its closure environment. The function body can access captured variables through projections from this environment parameter. This flat structure maps directly to the function model of assembly language, where each function has a fixed address and explicit parameters.

## Free Variable Analysis

Closure conversion requires identifying which variables each function captures from its enclosing scope. This free variable analysis traverses each function body to determine which variables are referenced but not bound within the function itself.

```rust
#![function!("system-f-omega/src/codegen/erase.rs", Erased::free_vars)]
```

The free variable analysis maintains a set of bound variables as it traverses expressions. Variables that appear in the expression but not in the bound set are free and must be captured in the function's closure. This analysis handles the scoping rules correctly, adding variables to the bound set when entering their scope and removing them when leaving.

## Environment Representation

The closure conversion algorithm maintains an environment that maps variables to their locations in closure environments. When a function captures variables, they are assigned positions in its closure, and references to these variables become projections at the appropriate indices.

```rust
#![function!("system-f-omega/src/codegen/closure.rs", ClosureConverter::convert_with_modules)]
```

The conversion process transforms each expression based on its structure and the current environment. Lambda abstractions become closure allocations that capture the current values of their free variables. Variable references check whether the variable is in the local environment or needs to be accessed through closure projection. Applications become calls that pass both the closure and the argument, following the calling convention for closure-converted code.

## Cranelift Code Generation

With closure conversion complete, the program consists of a collection of first-order functions and explicit closure operations. The Cranelift compiler framework transforms this representation into optimized machine code for the target architecture.

```rust
#![struct!("system-f-omega/src/codegen/compile.rs", CodeGen)]
```

The code generator maintains the state necessary for compilation, including the Cranelift module that accumulates compiled functions, the function builder context for constructing individual functions, and mappings between our function identifiers and Cranelift's function references. The runtime functions structure provides access to the runtime system primitives that support memory allocation and other essential operations.

## Value Representation

Our implementation uses a uniform representation for all runtime values, enabling polymorphic functions to operate on values of any type. This representation uses tagged pointers where the low bits indicate the value's type and the high bits contain the actual data or pointer.

The tagging scheme exploits the fact that heap-allocated objects are word-aligned, leaving the low bits available for type tags. Here's how different values are represented in our 64-bit system:

**Integer Representation**:
```
Original integer: 42

Binary representation of 42:      0000000000101010
Shift left by 3:                  0000000101010000
Add tag 1:                        0000000101010001

64-bit tagged value:
┌─────────────────────────────────────────────────────────┬───┐
│                     Value (42)                          │001│
└─────────────────────────────────────────────────────────┴───┘
 63                                                      3 2 0
                                                           tag
```

**Closure Representation**:
```
Closure in heap at address 0x7fff8000:

Heap memory layout:
┌────────────────┬────────────────┬────────────────┬────────────────┐
│ Code pointer   │ Environment    │ Captured       │ Captured       │
│ 0x400500       │ size: 2        │ value 1        │ value 2        │
└────────────────┴────────────────┴────────────────┴────────────────┘
 0x7fff8000       0x7fff8008       0x7fff8010       0x7fff8018

Tagged pointer (address already 8-byte aligned, tag 0):
┌─────────────────────────────────────────────────────────┬───┐
│                 0x7fff8000                              │000│
└─────────────────────────────────────────────────────────┴───┘
 63                                                      3 2 0
                                                           tag
```

**Extracting Values**:
```
To check if integer:     value & 0x7 == 1
To extract integer:      value >> 3 (arithmetic shift)
To extract pointer:      value & ~0x7 (clear low 3 bits)
```

**Examples**:
```
Integer -5:
  Actual value:     -5
  Binary:           1111111111111111111111111111111111111111111111111111111111111011
  Shifted left 3:   1111111111111111111111111111111111111111111111111111111111011000
  Tagged:           1111111111111111111111111111111111111111111111111111111111011001
  Hex:              0xffffffffffffffd9

Integer 1000:
  Tagged decimal:   8001
  Tagged hex:       0x1f41
  Extract:          8001 >> 3 = 1000

Closure at 0x7000:
  Tagged:           0x7000 (low bits already 000)
  Code pointer:     *(uint64_t*)0x7000
  Environment:      (uint64_t*)(0x7000 + 8)
```

This representation allows our runtime to efficiently distinguish between integers and heap-allocated objects using a single bit test, while keeping integers unboxed for performance. The 3-bit tag space could be extended to support additional immediate types like booleans or characters, though our current implementation only uses integers and pointers.

## Function Compilation

Each closure-converted function compiles to a Cranelift function that follows a uniform calling convention. Functions receive two parameters: the closure containing captured variables and the function argument. They return a single value using the same tagged representation.

```rust
#![function!("system-f-omega/src/codegen/compile.rs", CodeGen::compile_function)]
```

Function compilation begins by creating the appropriate function signature and setting up the entry block. The function parameters are bound to variables in the compilation environment, making them available for use in the function body. The body compilation generates instructions that compute the function result, which is then returned using Cranelift's return instruction.

## Expression Compilation

The expression compiler transforms closure-converted expressions into sequences of Cranelift instructions. Each expression type requires specific handling to generate correct and efficient machine code.

```rust
#![function!("system-f-omega/src/codegen/compile.rs", compile_expr)]
```

Variable compilation looks up the variable in the environment to find its Cranelift value. Integer literals are tagged and returned directly. Binary operations extract integer values from their tagged representations, perform the operation, and retag the result. Closure creation allocates memory for the closure structure and initializes it with captured values. Function calls invoke the target function with the appropriate closure and argument.

## Runtime System

The generated machine code cannot stand alone. It requires a runtime system that provides essential services not expressible in our high-level functional language. This runtime bridges the gap between pure functional code and the underlying operating system.

```rust
#![struct!("system-f-omega/src/codegen/runtime.rs", RuntimeFunctions)]
```

The runtime provides several categories of essential services through carefully designed functions that follow our calling conventions and value representations. Each runtime function is declared to Cranelift and can be called directly from generated code.

The actual implementation of these runtime functions lives in a separate support library written in Rust with `no_std` to minimize dependencies:

```rust
#![source_file!("system-f-omega/src/codegen/runtime_support.rs")]
```

The runtime support library serves several critical purposes:

**Memory Allocation**: Our functional language creates closures and other data structures dynamically, but has no concept of memory management. The runtime provides `rt_alloc`, a simple bump allocator that manages a fixed 1MB heap. This allocator is deliberately minimal - it only allocates, never frees, which is sufficient for our demonstration language. The bump allocator maintains a pointer into a static array and advances it for each allocation, ensuring 8-byte alignment for all allocations.

**Value Creation**: The `make_int` function tags raw integers for use in our tagged representation, while `make_closure` allocates and initializes closure structures with their code pointers and captured environments. The `project_env` function extracts values from closure environments during execution.

**Function Application**: The `apply` function implements the calling convention for closure invocation. It extracts the code pointer from a closure and calls it with the closure and argument, handling the low-level details of our calling convention.

**Input/Output**: Pure functional languages have no inherent notion of effects like printing to the console. The runtime provides `rt_print_int` which unwraps our tagged integer representation and calls the C library's `printf` function to display the value. This is our only connection to the outside world, allowing programs to produce observable output.

**Error Handling**: When the heap is exhausted, the runtime writes an error message directly to stderr using the POSIX `write` system call and terminates the program. The panic handler similarly ensures clean termination if any runtime invariant is violated.

**Operating System Interface**: The runtime uses direct FFI (Foreign Function Interface) calls to libc for its interactions with the operating system. This includes `printf` for formatted output, `write` for error messages, and `exit` for program termination. By using these standard C library functions, our runtime remains portable across any POSIX-compliant system.

The `no_std` and `no_main` attributes tell Rust not to include its standard library or generate a main function, keeping the runtime minimal. The entire runtime compiles to a small object file that gets linked with our generated code. During the build process, `build.rs` invokes the Rust compiler directly:

```bash
rustc --crate-type=staticlib --emit=obj -C opt-level=2 -C panic=abort runtime_support.rs
```

This produces an object file containing just our runtime functions, which the system linker combines with the Cranelift-generated code to create the final executable. The beauty of this approach is that we get exactly the runtime support we need - no more, no less - while leveraging existing system libraries for the heavy lifting.

## Executable Generation

The final phase links the generated code with the runtime system to produce a standalone executable. This process involves generating object files, linking with the runtime support library, and producing an executable for the target platform.

```rust
#![function!("system-f-omega/src/codegen/executable.rs", compile_executable)]
```

The executable generation process coordinates the entire compilation pipeline. Type erasure removes type information from the core language representation. Closure conversion transforms nested functions into explicit closures. Cranelift compilation generates machine code for each function. The main function wraps the program's entry point with appropriate initialization. Finally, the system linker combines the generated code with the runtime library to produce an executable program.

And that's it, that's a full compiler! You've made a full optimizing compiler from a pure platonic mathematical construct all the way down to the messy real world of silicon and electrons.

## Compiling a Simple Program

So how do invoke it? Basically just like any other compile. We take an input file and we get a machine-native executable out the other end.

Let's trace through the compilation of a simple recursive factorial function to see how all these pieces work together:

```haskell
-- factorial.fun
fib :: Int -> Int;
fib n = if n Le 1 then n else fib (n Sub 1) Add fib (n Sub 2);

main :: Int;
main = printInt(fib 10);
```

First, compile and run the program:

```bash
$ cd system-f-omega
$ cargo run -- compile factorial.fun -o factorial
$ ./factorial
55
```

The compilation process transforms this high-level program through several stages:

**After Type Erasure**: The polymorphic type information disappears, leaving only the computational structure:
```
fib = λn. if n Le 1 then n else fib (n Sub 1) Add fib (n Sub 2)
main = printInt(fib 10)
```

**After Closure Conversion**: Functions become explicit closures with captured environments:
```
Function 0: n -> if n Le 1 then n else closure(0, [])(n Sub 1) Add closure(0, [])(n Sub 2)
  Free vars: []
Main: printInt(closure(0, [])(10))
```

**Machine Code Generation**: Cranelift generates optimized assembly for the target architecture, handling the tagged value representation, closure allocation, and function calls according to our calling convention.

And that's the entire journey from theoretical type system to efficient machine code. The lambda calculus need not remain an abstract concept; it can be compiled down to run precisely as efficiently as C or Rust code with the right set of abstractions! Compilers need not be hard, in fact they are quite fun!
