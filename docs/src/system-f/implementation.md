# Implementation

Ok, now we move beyond the trivial type systems from the 1970s and into the fun stuff from the 1980s! System F's bidirectional type checking represents a  approach to handling polymorphic types without requiring full type annotations everywhere. The bidirectional approach splits type checking into two complementary modes: **inference** (synthesizing types from expressions) and **checking** (verifying that expressions conform to expected types). This division allows the system to gracefully handle situations where types are partially known or completely unknown, making the language more ergonomic while preserving type safety.

## Typing Rules

Before diving into the implementation details, let's establish the formal typing rules that govern System F. Buckle up, because we're about to embark into the fun magical land of type-level wizardry! We'll be introducing a few new symbols that might look intimidating at first, but they're really not that scary once you get used to them.

* **\\( \Gamma \\) (Gamma)** - The typing context, which is like a dictionary that maps variables to their types and keeps track of what we know so far.

* **\\( \vdash \\) (Turnstile)** - The "proves" or "entails" symbol. When we write \\( \Gamma \vdash e \Rightarrow A \\), we're saying "given context \\( \Gamma \\), expression \\( e \\) synthesizes type \\( A \\)."

* **\\( \Rightarrow \\) (Double Right Arrow)** - Inference mode, where we're asking "what type does this expression have?" The type checker figures it out for us.

* **\\( \Leftarrow \\) (Double Left Arrow)** - Checking mode, where we're saying "please verify this expression has the expected type." We already know what type we want.

* **\\( \forall \\) (Forall)** - Universal quantification, meaning "for any type." When we see \\( \forall \alpha. A \\), it means "for any type \\( \alpha \\), we have type \\( A \\)."

* **\\( \hat{\alpha} \\) (Hat Alpha)** - Existential type variables, which are like type-level unknowns that the system solves during inference. Think of them as placeholders that get filled in later.

* **\\( \bullet \\) (Bullet)** - The application judgment symbol used in our inference rules. When we write \\( A \bullet e \Rightarrow B \\), we're saying "applying type \\( A \\) to expression \\( e \\) yields type \\( B \\)."

* **\\( <: \\) (Subtype)** - The subtyping relation, expressing that one type is "more specific" than another. For example, \\( \text{Int} <: \forall \alpha. \alpha \\) would mean Int is a subtype of the polymorphic type.

* **\\( [B/\alpha]A \\)** - Type substitution, replacing all occurrences of type variable \\( \alpha \\) with type \\( B \\) in type \\( A \\). This is how we instantiate polymorphic types.

Now that we've equipped ourselves with this symbolic toolkit, let's see how these pieces combine to create the elegant machinery of System F type checking.

### Basic Rules

The variable rule looks up types from the context:

\\[ \frac{x : A \in \Gamma}{\Gamma \vdash x \Rightarrow A} \text{(T-Var)} \\]

Application checks that the function type matches the argument:

\\[ \frac{\Gamma \vdash e_1 \Rightarrow A \to B \quad \Gamma \vdash e_2 \Leftarrow A}{\Gamma \vdash e_1 \; e_2 \Rightarrow B} \text{(T-App)} \\]

Lambda abstraction introduces a new variable binding:

\\[ \frac{\Gamma, x : A \vdash e \Leftarrow B}{\Gamma \vdash \lambda x. e \Leftarrow A \to B} \text{(T-Abs)} \\]

### Polymorphic Rules

Universal introduction allows us to generalize over type variables:

\\[ \frac{\Gamma, \alpha \vdash e \Leftarrow A}{\Gamma \vdash e \Leftarrow \forall \alpha. A} \text{(T-ForallI)} \\]

Universal elimination instantiates polymorphic types:

\\[ \frac{\Gamma \vdash e \Rightarrow \forall \alpha. A}{\Gamma \vdash e \Rightarrow [B/\alpha]A} \text{(T-ForallE)} \\]

Type annotation allows switching from checking to inference mode:

\\[ \frac{\Gamma \vdash e \Leftarrow A}{\Gamma \vdash (e : A) \Rightarrow A} \text{(T-Instr)} \\]

### Primitive Type Rules

Integer literals have type Int:

\\[ \frac{}{\Gamma \vdash n \Rightarrow \text{Int}} \text{(T-LitInt)} \\]

Boolean literals have type Bool:

\\[ \frac{}{\Gamma \vdash \text{true} \Rightarrow \text{Bool}} \text{(T-LitBool)} \\]

### Control Flow Rules

Let bindings introduce local variables:

\\[ \frac{\Gamma \vdash e_1 \Rightarrow A \quad \Gamma, x : A \vdash e_2 \Rightarrow B}{\Gamma \vdash \text{let } x = e_1 \text{ in } e_2 \Rightarrow B} \text{(T-Let)} \\]

Conditional expressions require Bool conditions and matching branch types:

\\[ \frac{\Gamma \vdash e_1 \Leftarrow \text{Bool} \quad \Gamma \vdash e_2 \Rightarrow A \quad \Gamma \vdash e_3 \Leftarrow A}{\Gamma \vdash \text{if } e_1 \text{ then } e_2 \text{ else } e_3 \Rightarrow A} \text{(T-If)} \\]

### Binary Operation Rules

Arithmetic operations take two integers and return an integer:

\\[ \frac{\Gamma \vdash e_1 \Leftarrow \text{Int} \quad \Gamma \vdash e_2 \Leftarrow \text{Int}}{\Gamma \vdash e_1 \oplus e_2 \Rightarrow \text{Int}} \text{(T-Arith)} \\]

Boolean operations take two booleans and return a boolean:

\\[ \frac{\Gamma \vdash e_1 \Leftarrow \text{Bool} \quad \Gamma \vdash e_2 \Leftarrow \text{Bool}}{\Gamma \vdash e_1 \land e_2 \Rightarrow \text{Bool}} \text{(T-Bool)} \\]

Comparison operations take two integers and return a boolean:

\\[ \frac{\Gamma \vdash e_1 \Leftarrow \text{Int} \quad \Gamma \vdash e_2 \Leftarrow \text{Int}}{\Gamma \vdash e_1 < e_2 \Rightarrow \text{Bool}} \text{(T-Cmp)} \\]

Equality operations are polymorphic and work on any type:

\\[ \frac{\Gamma \vdash e_1 \Rightarrow A \quad \Gamma \vdash e_2 \Leftarrow A}{\Gamma \vdash e_1 = e_2 \Rightarrow \text{Bool}} \text{(T-Eq)} \\]

### Bidirectional Rules

The mode switch allows inference results to be checked:

\\[ \frac{\Gamma \vdash e \Rightarrow A}{\Gamma \vdash e \Leftarrow A} \text{(T-Sub)} \\]

Existential variables are introduced for unknown types:

\\[ \frac{\Gamma, \hat{\alpha} \vdash e \Rightarrow A}{\Gamma \vdash e \Rightarrow [\hat{\alpha}/\alpha]A} \text{(T-InstL)} \\]

### Application Inference Rules

Application inference handles complex cases where the function type is not immediately known:

Application with arrow types:
\\[ \frac{\Gamma \vdash e_2 \Leftarrow A}{\Gamma \vdash A \to B \bullet e_2 \Rightarrow B} \text{(T-AppArrow)} \\]

Application with existential variables:
\\[ \frac{\Gamma[\hat{\alpha} := \hat{\alpha_1} \to \hat{\alpha_2}], \hat{\alpha_1}, \hat{\alpha_2} \vdash e_2 \Leftarrow \hat{\alpha_1}}{\Gamma \vdash \hat{\alpha} \bullet e_2 \Rightarrow \hat{\alpha_2}} \text{(T-AppEVar)} \\]

In these rules, \\( \Rightarrow \\) indicates **inference** mode (synthesizing a type), while \\( \Leftarrow \\) indicates **checking** mode (verifying against an expected type). The hat notation \\( \hat{\alpha} \\) denotes existential type variables that the system solves during inference.

## Core Data Structures

### Context and Environment Management

The bidirectional algorithm maintains a  context that tracks multiple kinds of bindings and constraints. Our context system needs to handle not just term variables and their types, but also type variables, existential variables, and the relationships between them.

```rust
#![enum!("system-f/src/typecheck.rs", Entry)]
```

The context entries represent different kinds of information the type checker needs to track throughout the bidirectional inference process. Variable bindings, represented as `VarBnd(TmVar, Type)`, associate term variables with their types, such as recording that `x` has type `Int` in the current scope. Type variable bindings, denoted as `TVarBnd(TyVar)`, introduce type variables into scope, such as the \\( \\alpha \\) that appears in universal quantification \\( \\forall \\alpha. \\ldots \\).

Existential type variable bindings, written as `ETVarBnd(TyVar)`, introduce existential type variables that represent unknown types to be determined through constraint solving. Solved existential type variable bindings, represented as `SETVarBnd(TyVar, Type)`, record the concrete solutions discovered for existential variables during the inference process. Finally, marker entries, denoted as `Mark(TyVar)`, mark the beginning of a scope to enable proper garbage collection of type variables when their scope is exited.

The context itself maintains these entries in a stack-like structure where order matters crucially for scoping and variable resolution:

```rust
#![struct!("system-f/src/typecheck.rs", Context)]
```

Context management requires careful attention to scoping rules. When we enter a polymorphic function or introduce new type variables, we must ensure they are properly cleaned up when we exit their scope. The context provides methods for breaking it apart and reconstructing it to handle these operations efficiently.

### Type Substitution and Application

Type substitution forms the computational heart of our System F implementation. When we instantiate a polymorphic type or solve existential variables, we need to systematically replace type variables with concrete types throughout complex type expressions.

```rust
#![function!("system-f/src/typecheck.rs", BiDirectional::subst_type)]
```

Substitution must handle variable capture correctly. When substituting into a `Forall` type, we must ensure that the bound variable doesn't conflict with the replacement type. This is analogous to alpha-conversion in lambda calculus but operates at the type level.

Context application extends substitution by applying the current state of existential variable solutions to a type:

```rust
#![function!("system-f/src/typecheck.rs", BiDirectional::apply_ctx_type)]
```

This operation is essential because our algorithm incrementally builds up solutions to existential variables. As we learn more about unknown types, we need to propagate this information through all the types we're working with.

## Bidirectional Algorithm Core

### Inference Rules

The inference mode synthesizes types from expressions. Our implementation uses a modular approach where the main inference function delegates to specialized methods for each syntactic form.

```rust
#![function!("system-f/src/typecheck.rs", BiDirectional::infer)]
```

The inference function delegates to specialized methods that implement individual typing rules:

**Variable Lookup** follows the T-Var rule:

\\[ \frac{x : A \in \Gamma}{\Gamma \vdash x \Rightarrow A} \text{(T-Var)} \\]

```rust
#![function!("system-f/src/typecheck.rs", BiDirectional::infer_var)]
```

**Integer Literals** use the T-LitInt rule:

\\[ \frac{}{\Gamma \vdash n \Rightarrow \text{Int}} \text{(T-LitInt)} \\]

```rust
#![function!("system-f/src/typecheck.rs", BiDirectional::infer_lit_int)]
```

**Boolean Literals** use the T-LitBool rule:

\\[ \frac{}{\Gamma \vdash \text{true} \Rightarrow \text{Bool}} \text{(T-LitBool)} \\]

```rust
#![function!("system-f/src/typecheck.rs", BiDirectional::infer_lit_bool)]
```

**Lambda Abstraction** implements the T-Abs rule:

\\[ \frac{\Gamma, x : A \vdash e \Leftarrow B}{\Gamma \vdash \lambda x. e \Leftarrow A \to B} \text{(T-Abs)} \\]

```rust
#![function!("system-f/src/typecheck.rs", BiDirectional::infer_abs)]
```

**Function Application** uses the T-App rule:

\\[ \frac{\Gamma \vdash e_1 \Rightarrow A \to B \quad \Gamma \vdash e_2 \Leftarrow A}{\Gamma \vdash e_1 \; e_2 \Rightarrow B} \text{(T-App)} \\]

```rust
#![function!("system-f/src/typecheck.rs", BiDirectional::infer_application)]
```

**Let Bindings** implement the T-Let rule:

\\[ \frac{\Gamma \vdash e_1 \Rightarrow A \quad \Gamma, x : A \vdash e_2 \Rightarrow B}{\Gamma \vdash \text{let } x = e_1 \text{ in } e_2 \Rightarrow B} \text{(T-Let)} \\]

```rust
#![function!("system-f/src/typecheck.rs", BiDirectional::infer_let)]
```

**Conditional Expressions** use the T-If rule:

\\[ \frac{\Gamma \vdash e_1 \Leftarrow \text{Bool} \quad \Gamma \vdash e_2 \Rightarrow A \quad \Gamma \vdash e_3 \Leftarrow A}{\Gamma \vdash \text{if } e_1 \text{ then } e_2 \text{ else } e_3 \Rightarrow A} \text{(T-If)} \\]

```rust
#![function!("system-f/src/typecheck.rs", BiDirectional::infer_if)]
```

**Binary Operations** implement the T-BinOp rules (T-Arith, T-Bool, T-Cmp, T-Eq):

\\[ \frac{\Gamma \vdash e_1 \Leftarrow \text{Int} \quad \Gamma \vdash e_2 \Leftarrow \text{Int}}{\Gamma \vdash e_1 \oplus e_2 \Rightarrow \text{Int}} \text{(T-Arith)} \\]

\\[ \frac{\Gamma \vdash e_1 \Leftarrow \text{Bool} \quad \Gamma \vdash e_2 \Leftarrow \text{Bool}}{\Gamma \vdash e_1 \land e_2 \Rightarrow \text{Bool}} \text{(T-Bool)} \\]

\\[ \frac{\Gamma \vdash e_1 \Leftarrow \text{Int} \quad \Gamma \vdash e_2 \Leftarrow \text{Int}}{\Gamma \vdash e_1 < e_2 \Rightarrow \text{Bool}} \text{(T-Cmp)} \\]

\\[ \frac{\Gamma \vdash e_1 \Rightarrow A \quad \Gamma \vdash e_2 \Leftarrow A}{\Gamma \vdash e_1 = e_2 \Rightarrow \text{Bool}} \text{(T-Eq)} \\]

```rust
#![function!("system-f/src/typecheck.rs", BiDirectional::infer_binop)]
```

**Type Annotations** use the T-Instr rule:

\\[ \frac{\Gamma \vdash e \Leftarrow A}{\Gamma \vdash (e : A) \Rightarrow A} \text{(T-Instr)} \\]

```rust
#![function!("system-f/src/typecheck.rs", BiDirectional::infer_ann)]
```

**Type Abstraction** implements the T-TAbs rule:

\\[ \frac{\Gamma, \alpha \vdash e \Rightarrow A}{\Gamma \vdash \Lambda \alpha. e \Rightarrow \forall \alpha. A} \text{(T-TAbs)} \\]

```rust
#![function!("system-f/src/typecheck.rs", BiDirectional::infer_tabs)]
```

**Type Application** uses the T-TApp rule:

\\[ \frac{\Gamma \vdash e \Rightarrow \forall \alpha. A}{\Gamma \vdash e[B] \Rightarrow [B/\alpha]A} \text{(T-TApp)} \\]

```rust
#![function!("system-f/src/typecheck.rs", BiDirectional::infer_tapp)]
```

Each method includes comments linking it to the formal typing rule it implements, making the correspondence between theory and implementation explicit.

### Checking Rules

The checking mode verifies that expressions conform to expected types. This is where the algorithm can make progress even when types aren't fully determined.

```rust
#![function!("system-f/src/typecheck.rs", BiDirectional::check)]
```

Checking mode provides specialized handling that takes advantage of known type information to guide the inference process more efficiently. For lambda expressions, when checking \\( \\lambda x:\\tau_1. e \\) against type \\( \\tau_1 \\to \\tau_2 \\), the algorithm can immediately verify that the parameter types match and then recursively check the body \\( e \\) against the expected return type \\( \\tau_2 \\). This direct decomposition avoids the need to synthesize types and then verify compatibility.

When checking against universal types \\( \\forall\\alpha. \\tau \\), the algorithm introduces the type variable \\( \\alpha \\) into the context and then checks the expression against the instantiated type \\( \\tau \\). This approach ensures that the universal quantification is handled correctly while maintaining the scoping discipline required for sound type checking. When direct checking strategies are not applicable to the current expression and expected type combination, the algorithm falls back to a synthesis-plus-subtyping approach, where it first synthesizes a type for the expression and then verifies that this synthesized type is a subtype of the expected type.

## Subtyping and Instantiation

### Subtyping Relations

System F includes a  subtyping system that handles the relationships between polymorphic types. The key insight is that `∀α. τ` is more general than any specific instantiation of `τ`.

```rust
#![function!("system-f/src/typecheck.rs", BiDirectional::subtype)]
```

The subtyping rules capture several essential relationships that govern type compatibility in System F. Function types exhibit the classic contravariant-covariant pattern, where a function that accepts more general arguments and returns more specific results is considered a subtype of a function with more specific argument requirements and more general return types. This means that a function of type \\( A_1 \\to B_1 \\) is a subtype of \\( A_2 \\to B_2 \\) when \\( A_2 \\leq A_1 \\) (contravariant in arguments) and \\( B_1 \\leq B_2 \\) (covariant in results).

Universal quantification follows the principle that \\( \\forall\\alpha. \\tau_1 \\leq \\tau_2 \\) holds if \\( \\tau_1 \\leq \\tau_2 \\) when \\( \\alpha \\) is instantiated with a fresh existential variable, effectively allowing polymorphic types to be related through their instantiations. When subtyping involves existential variables, the algorithm must solve constraints about what these variables should be instantiated to in order to satisfy the subtyping relationship, often leading to the generation of additional constraints that propagate through the system.

### Variable Instantiation

The instantiation judgments handle the core complexity of polymorphic type checking. When we have constraints like `^α ≤ τ` (an existential variable should be at most as general as some type), we need to find appropriate instantiations.

```rust
#![function!("system-f/src/typecheck.rs", BiDirectional::inst_l)]
```

Left instantiation (`inst_l`) handles cases where the existential variable is on the left side of a constraint. This typically means we're looking for the most general type that satisfies the constraint.

```rust
#![function!("system-f/src/typecheck.rs", BiDirectional::inst_r)]
```

Right instantiation (`inst_r`) handles the dual case where the existential variable is on the right. This typically means we're looking for the most specific type that satisfies the constraint.

The instantiation algorithms include careful handling of several complex scenarios that arise during constraint solving. The reach relationship occurs when two existential variables are constrained against each other, requiring the algorithm to determine which variable should be solved in terms of the other while maintaining the proper ordering constraints. Arrow type instantiation requires breaking function types apart into their component argument and return types, creating separate instantiation constraints for each component that must be solved consistently.

The interaction between instantiation and universal quantification presents particular challenges, as the algorithm must ensure that polymorphic types are instantiated correctly while preserving the scoping discipline that prevents type variables from escaping their intended scope. These cases require  constraint management to ensure that all relationships are maintained throughout the solving process.

### Occurs Check

A critical component of sound type inference is the occurs check, which prevents infinite types from arising during unification. When solving constraints like `^α := τ`, we must ensure that `α` does not occur within `τ`, as this would create a cyclic type.

```rust
#![function!("system-f/src/typecheck.rs", BiDirectional::occurs_check)]
```

The occurs check is applied during the InstLSolve and InstRSolve cases of instantiation. Without this check, the type system could accept programs that would lead to infinite types, violating the decidability of type checking.

### Application Inference

Function application in System F requires careful handling because the function type might not be immediately apparent. The `infer_app` judgment implements the T-AppArrow and T-AppEVar rules defined earlier:

```rust
#![function!("system-f/src/typecheck.rs", BiDirectional::infer_app)]
```

The implementation handles two core application scenarios through distinct inference rules. The T-AppArrow rule applies when the function has a known arrow type \\( A \\to B \\), allowing the algorithm to check the argument against \\( A \\) and return \\( B \\) as the result type. This straightforward case corresponds to the `Type::Arrow` pattern in the implementation and represents the standard function application scenario.

The T-AppEVar rule handles the more complex case where the function type is an existential variable \\( \\hat{\\alpha} \\). In this situation, the algorithm instantiates the existential variable as \\( \\hat{\\alpha_1} \\to \\hat{\\alpha_2} \\) with fresh existential variables, then checks the argument against \\( \\hat{\\alpha_1} \\) and returns \\( \\hat{\\alpha_2} \\) as the result type. This corresponds to the `Type::ETVar` case and enables type inference even when the function type is initially unknown.

When the function has a polymorphic `Forall` type, the instantiation is handled through the subtyping mechanism using the SubAllL rule rather than directly in application inference. This design choice ensures soundness by routing polymorphic instantiation through the well-established subtyping infrastructure and follows the standard bidirectional algorithm design patterns.

## Error Handling

Our implementation provides comprehensive error reporting that distinguishes between parse errors and type errors. Parse errors use source-located reporting with ariadne, while type errors provide contextual information about the expressions being typed.

```rust
#![enum!("system-f/src/errors.rs", TypeError)]
```

The type error system includes expression context to help developers understand where failures occur during type checking. Each error variant includes an `expr` field that stores the expression being typed when the error occurred, providing valuable debugging information.

Parse errors receive enhanced treatment with source location information for precise error reporting:

```rust
#![enum!("system-f/src/errors.rs", ParseError)]
```

This dual approach ensures that syntax errors receive precise source location feedback while type errors focus on logical relationships between expressions and types.

## End-to-end

To demonstrate the complete System F implementation with all the modular typing rules working together, consider this lambda expression that doubles its integer argument:

```bash
$ cargo run -- check "\x : Int -> x + x"
```

The system produces the following output showing the complete inference process:

```
Parsed expression: Abs("x", Int, BinOp(Add, Var("x"), Var("x")))

Type checking successful!
Final type: Int -> Int

InfLam:  ⊢ Abs("x", Int, BinOp(Add, Var("x"), Var("x"))) =>  ⊢ Abs("x", Int, BinOp(Add, Var("x"), Var("x"))) ⇒ Int -> ^α0 ⊣ ^α0 = Int
  ChkSub: ^α0, x: Int ⊢ BinOp(Add, Var("x"), Var("x")) ⇐ ^α0 => ^α0 = Int, x: Int
    InfArith: ^α0, x: Int ⊢ BinOp(Add, Var("x"), Var("x")) => ^α0, x: Int ⊢ BinOp(Add, Var("x"), Var("x")) ⇒ Int ⊣ ^α0, x: Int
      ChkSub: ^α0, x: Int ⊢ Var("x") ⇐ Int => ^α0, x: Int
        InfVar: ^α0, x: Int ⊢ Var("x") => ^α0, x: Int ⊢ Var("x") ⇒ Int ⊣ ^α0, x: Int
        SubRefl: ^α0, x: Int ⊢ Int <: Int => ^α0, x: Int
      ChkSub: ^α0, x: Int ⊢ Var("x") ⇐ Int => ^α0, x: Int
        InfVar: ^α0, x: Int ⊢ Var("x") => ^α0, x: Int ⊢ Var("x") ⇒ Int ⊣ ^α0, x: Int
        SubRefl: ^α0, x: Int ⊢ Int <: Int => ^α0, x: Int
    SubInstR: ^α0, x: Int ⊢ Int <: ^α0 => ^α0 = Int, x: Int
      InstRSolve: ^α0, x: Int ⊢ Int :=< ^α0 => ^α0 = Int, x: Int
```

The final result shows `Final type: Int -> Int`, correctly inferring that this lambda expression is a function from integers to integers! The existential variable `^α0` in the proof tree gets resolved to `Int` through the constraint solving process, and the final type application ensures all existential variables are properly substituted in the output.

For non-trivial expressions, the constraint solving process may involve more complex reasoning and may require additional inference steps but using the proof tree method we can visualize the inference steps and understand the constraints being solved. This is a very powerful technique.

## Mission Accomplished

And there we have it! A complete bidirectional type checker for System F with polymorphic types, existential variables, and  constraint solving! The algorithm handles the complex interplay between synthesis and checking modes, managing existential variables and subtyping relationships with the precision needed for a production-quality type system.

The bidirectional approach transforms what could be an overwhelming inference problem into a systematic, decidable process that gives us both powerful expressiveness and reliable type safety. System F's polymorphism opens up a whole new world of type-safe programming that feels almost magical once you see it in action!
