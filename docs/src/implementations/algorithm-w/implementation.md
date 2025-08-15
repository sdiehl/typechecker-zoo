# Core Implementation

Algorithm W represents one of the most elegant solutions to the type inference problem in functional programming languages. Developed by Robin Milner in 1978, it provides a sound and complete method for inferring the most general types in the [Hindley-Milner type system](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system). This chapter explores our Rust implementation, examining how the mathematical foundations translate into practical code that can handle lambda abstractions, function applications, let-polymorphism, and complex unification scenarios.

The core insight of Algorithm W lies in its systematic approach to type inference through constraint generation and unification. Rather than attempting to determine types through local analysis, the algorithm builds a global picture by generating type variables, collecting constraints, and then solving these constraints through unification. This approach ensures that we always find the most general type possible, a property crucial for supporting polymorphism in functional languages.

You'll often see this algorithm (or the type system, confusingly enough) referred to by many names:

* **Hindley-Milner**
* **Hindley-Damas-Milner**
* **Damas-Milner**
* **HM**
* **Algorithm W**

## Typing Rules

Before diving into the implementation details, let's establish the formal typing rules that govern the Hindley-Milner type system. We'll be introducing mathematical symbols that capture the essence of type inference, but don't worry, each symbol has a precise and intuitive meaning once you dive into the details.

* **\\( \Gamma \\) (Gamma)** - The type environment, which maps variables to their types. It's like a dictionary that remembers what we know about each variable's type.

* **\\( \vdash \\) (Turnstile)** - The "entails" or "proves" symbol. When we write \\( \Gamma \vdash e : \tau \\), we're saying "in environment \\( \Gamma \\), expression \\( e \\) has type \\( \tau \\)."

* **\\( \tau \\) (Tau)** - Represents monomorphic types like \\( \text{Int} \\), \\( \text{Bool} \\), or \\( \text{Int} \to \text{Bool} \\). These are concrete, fully-determined types.

* **\\( \sigma \\) (Sigma)** - Represents polymorphic type schemes like \\( \forall \alpha. \alpha \to \alpha \\). These can be instantiated with different concrete types.

* **\\( \forall \alpha \\) (Forall Alpha)** - Universal quantification over type variables. It means "for any type \\( \alpha \\)." This is how we express polymorphism.

* **\\( \alpha, \beta, \gamma \\) (Greek Letters)** - Type variables that stand for unknown types during inference. Think of them as type-level unknowns that get solved.

* **\\( [\tau/\alpha]\sigma \\)** - Type substitution, replacing all occurrences of type variable \\( \alpha \\) with type \\( \tau \\) in scheme \\( \sigma \\). This is how we instantiate polymorphic types.

* **\\( S \\) (Substitution)** - A mapping from type variables to types, representing the solutions found by unification.

* **\\( \text{gen}(\Gamma, \tau) \\)** - Generalization, which turns a monotype into a polytype by quantifying over type variables not present in the environment.

* **\\( \text{inst}(\sigma) \\)** - Instantiation, which creates a fresh monotype from a polytype by replacing quantified variables with fresh type variables.

* **\\( \text{ftv}(\tau) \\)** - Free type variables, the set of unbound type variables appearing in type \\( \tau \\).

* **\\( \emptyset \\) (Empty Set)** - The empty substitution, representing no changes to types.

* **\\( [\alpha \mapsto \tau] \\)** - A substitution that maps type variable \\( \alpha \\) to type \\( \tau \\).

* **\\( S_1 \circ S_2 \\)** - Composition of substitutions, applying \\( S_2 \\) first, then \\( S_1 \\).

* **\\( \notin \\) (Not In)** - Set membership negation, used in the occurs check to prevent infinite types.

Now that we have our symbolic toolkit, let's see how these pieces work together to create the elegant machinery of Algorithm W.

### Core Typing Rules

The variable rule looks up types from the environment:
\\[ \\frac{x : σ \\in Γ \\quad τ = \\text{inst}(σ)}{Γ ⊢ x : τ} \\text{(T-Var)} \\]

Lambda abstraction introduces new variable bindings:
\\[ \\frac{Γ, x : α ⊢ e : τ \\quad α \\text{ fresh}}{Γ ⊢ λx. e : α → τ} \\text{(T-Lam)} \\]

Function application combines types through unification:
\\[ \\frac{Γ ⊢ e₁ : τ₁ \\quad Γ ⊢ e₂ : τ₂ \\quad α \\text{ fresh} \\quad S = \\text{unify}(τ₁, τ₂ → α)}{Γ ⊢ e₁ \\, e₂ : S(α)} \\text{(T-App)} \\]

Let-polymorphism allows generalization:
\\[ \\frac{Γ ⊢ e₁ : τ₁ \\quad σ = \\text{gen}(Γ, τ₁) \\quad Γ, x : σ ⊢ e₂ : τ₂}{Γ ⊢ \\text{let } x = e₁ \\text{ in } e₂ : τ₂} \\text{(T-Let)} \\]

Literals have their corresponding base types:
\\[ \\frac{}{Γ ⊢ n : \\text{Int}} \\text{(T-LitInt)} \\]

\\[ \\frac{}{Γ ⊢ b : \\text{Bool}} \\text{(T-LitBool)} \\]

These rules capture the essence of the Hindley-Milner type system, where we infer the most general types while supporting true polymorphism through let-generalization.

## Abstract Syntax Trees

Our implementation begins with a careful modeling of both expressions and types as algebraic data types. The expression language extends the pure lambda calculus with practical constructs while maintaining the theoretical foundation.

```rust
#![enum!("algorithm-w/src/ast.rs", Expr)]
```

The expression AST captures the essential constructs of our language. Variables (`Var`) and function abstractions (`Abs`) correspond directly to the lambda calculus. Function application (`App`) drives computation through beta reduction. The `Let` construct introduces local bindings with potential for polymorphic generalization, while literals (`Lit`) and tuples (`Tuple`) provide concrete data types that make the language practical for real programming tasks.

The type system mirrors this structure with its own AST that represents the types these expressions can have.

```rust
#![enum!("algorithm-w/src/ast.rs", Type)]
```

Type variables (`Type::Var`) serve as placeholders during inference, eventually getting instantiated to concrete types through unification. Arrow types (`Type::Arrow`) represent function types, encoding both parameter and return types. Base types like `Int` and `Bool` provide the foundation, while tuple types (`Type::Tuple`) support structured data. The recursive nature of these types allows us to express arbitrarily complex type structures, from simple integers to higher-order functions that manipulate other functions.

## Type Inference Algorithm

Algorithm W operates on several fundamental data structures that capture the essential concepts of type inference. These type aliases provide names for the core abstractions and make the algorithm's implementation more readable.

The type variable abstraction represents unknown types that will be resolved during inference. Term variables represent program variables that appear in expressions. The type environment maps term variables to their types, while substitutions map type variables to concrete types.

```rust
pub type TyVar = String;
pub type TmVar = String;
pub type Env = BTreeMap<TmVar, Scheme>;  // Now stores schemes, not types
pub type Subst = HashMap<TyVar, Type>;
```

These aliases encapsulate the fundamental data flow in Algorithm W. Type variables like `t0`, `t1`, and `t2` serve as placeholders that get unified with concrete types as inference progresses. Term variables represent the actual identifiers in source programs. The environment now tracks polymorphic type schemes rather than just types, enabling proper let-polymorphism, while substitutions record the solutions discovered by unification.

The choice of `String` for both type and term variables reflects the simplicity of our implementation. In a full implementation, systems often use more complex representations like de Bruijn indices for type variables or interned strings for performance, but strings provide clarity for understanding the fundamental algorithms.

The heart of our Algorithm W implementation lies in the `TypeInference` struct, which maintains the state necessary for sound type inference across an entire program.

```rust
#![struct!("algorithm-w/src/infer.rs", TypeInference)]
```

The inference engine's primary responsibility is generating fresh type variables, a process that ensures each unknown type gets a unique identifier. This counter-based approach provides a simple but effective way to avoid naming collisions during the inference process.

```rust
#![function!("algorithm-w/src/infer.rs", TypeInference::fresh_tyvar)]
```

Fresh variable generation forms the foundation for Algorithm W's systematic approach to handling unknowns. Each time we encounter an expression whose type we don't yet know, we assign it a fresh type variable. These variables later get unified with concrete types as we discover more information about the program's structure.

## Substitution and Unification

Type substitutions represent the core computational mechanism of Algorithm W. A substitution maps type variables to concrete types, effectively "solving" part of our type inference puzzle.

The application of substitutions must handle the recursive structure of types correctly, ensuring that substitutions propagate through compound types like arrows and tuples.

```rust
#![function!("algorithm-w/src/infer.rs", TypeInference::apply_subst)]
```

Substitution application demonstrates how type information flows through our system. When we apply a substitution to an arrow type, we must apply it recursively to both the parameter and return types. This ensures that type information discovered in one part of a program correctly influences other parts.

Composition of substitutions allows us to combine multiple partial solutions into a more complete understanding of our program's types.

```rust
#![function!("algorithm-w/src/infer.rs", TypeInference::compose_subst)]
```

The composition operation ensures that when we have multiple substitutions from different parts of our inference process, we can combine them into a single, consistent substitution that represents our cumulative knowledge about the program's types.

Substitutions must also be applied to entire type environments when we discover new type information. This operation updates all the types in the environment according to the current substitution.

```rust
#![function!("algorithm-w/src/infer.rs", TypeInference::apply_subst_env)]
```

Environment substitution is crucial for maintaining consistency as inference progresses. When we discover that a type variable should be instantiated to a concrete type, we must update not just individual types but entire environments to reflect this new knowledge.

### Unification

Unification is the heart of type inference, solving constraints between types. The unification algorithm produces substitutions that make two types equivalent:

Reflexivity - identical types unify trivially:
\\[ \\frac{}{\\text{unify}(τ, τ) = \\emptyset} \\text{(U-Refl)} \\]

Variable unification with occurs check:
\\[ \\frac{α \\notin \\text{ftv}(τ)}{\\text{unify}(α, τ) = [α ↦ τ]} \\text{(U-VarL)} \\]

\\[ \\frac{α \\notin \\text{ftv}(τ)}{\\text{unify}(τ, α) = [α ↦ τ]} \\text{(U-VarR)} \\]

Arrow type unification decomposes into domain and codomain:
\\[ \\frac{S₁ = \\text{unify}(τ₁, τ₃) \\quad S₂ = \\text{unify}(S₁(τ₂), S₁(τ₄))}{\\text{unify}(τ₁ → τ₂, τ₃ → τ₄) = S₂ ∘ S₁} \\text{(U-Arrow)} \\]

Tuple unification requires component-wise unification:
\\[ \\frac{S₁ = \\text{unify}(τ₁, τ₃) \\quad S₂ = \\text{unify}(S₁(τ₂), S₁(τ₄))}{\\text{unify}((τ₁, τ₂), (τ₃, τ₄)) = S₂ ∘ S₁} \\text{(U-Tuple)} \\]

Base type unification succeeds only for identical types:
\\[ \\frac{}{\\text{unify}(\\text{Int}, \\text{Int}) = \\emptyset} \\text{(U-Int)} \\]

\\[ \\frac{}{\\text{unify}(\\text{Bool}, \\text{Bool}) = \\emptyset} \\text{(U-Bool)} \\]

These unification rules ensure that type constraints are solved systematically while maintaining soundness through the occurs check.

When we have two types that must be equal (such as the parameter type of a function and the type of an argument being passed to it), unification determines whether this constraint can be satisfied and, if so, what substitution makes them equal.

```rust
#![function!("algorithm-w/src/infer.rs", TypeInference::unify)]
```

The unification algorithm handles several distinct cases, each representing a different constraint-solving scenario. When unifying two identical base types like `Int` with `Int`, no substitution is needed. When unifying a type variable with any other type, we create a substitution that maps the variable to that type, provided the occurs check passes.

The occurs check prevents infinite types by ensuring that a type variable doesn't appear within the type it's being unified with.

```rust
#![function!("algorithm-w/src/infer.rs", TypeInference::occurs_check)]
```

This check is essential for soundness. Without it, we might generate infinite types like `t0 = t0 -> Int`, which would break our type system's decidability.

For compound types like arrows, unification becomes recursive. We must unify corresponding subcomponents and then compose the resulting substitutions. This process ensures that complex types maintain their structural relationships while allowing for flexible instantiation of type variables.

## The Main Inference Algorithm

The central `infer` method implements Algorithm W proper, analyzing expressions to determine their types while accumulating the necessary substitutions. Our implementation uses a modular approach where each syntactic construct has its own helper method implementing the corresponding typing rule.

```rust
#![function!("algorithm-w/src/infer.rs", TypeInference::infer)]
```

Each helper method corresponds directly to a formal typing rule, making the relationship between theory and implementation explicit.

### Variable Lookup

\\[ \\frac{x : σ \\in Γ \\quad τ = \\text{inst}(σ)}{Γ ⊢ x : τ} \\text{(T-Var)} \\]

```rust
#![function!("algorithm-w/src/infer.rs", TypeInference::infer_var)]
```

Variable lookup requires instantiation of polymorphic types. When we find a variable in the environment, it might have a polymorphic type scheme like `∀α. α → α`. We create a fresh monomorphic instance by replacing quantified variables with fresh type variables.

### Lambda Abstraction

\\[ \\frac{Γ, x : α ⊢ e : τ \\quad α \\text{ fresh}}{Γ ⊢ λx. e : α → τ} \\text{(T-Lam)} \\]

```rust
#![function!("algorithm-w/src/infer.rs", TypeInference::infer_abs)]
```

Lambda abstractions introduce new variable bindings. We assign a fresh type variable to the parameter, extend the environment, and infer the body's type. Any constraints discovered during body inference get propagated back through substitution.

### Function Application

\\[ \\frac{Γ ⊢ e₁ : τ₁ \\quad Γ ⊢ e₂ : τ₂ \\quad α \\text{ fresh} \\quad S = \\text{unify}(τ₁, τ₂ → α)}{Γ ⊢ e₁ \\, e₂ : S(α)} \\text{(T-App)} \\]

```rust
#![function!("algorithm-w/src/infer.rs", TypeInference::infer_app)]
```

Application drives constraint generation. We infer types for both function and argument, then unify the function type with an arrow type constructed from the argument type and a fresh result type variable.

### Let-Polymorphism

\\[ \\frac{Γ ⊢ e₁ : τ₁ \\quad σ = \\text{gen}(Γ, τ₁) \\quad Γ, x : σ ⊢ e₂ : τ₂}{Γ ⊢ \\text{let } x = e₁ \\text{ in } e₂ : τ₂} \\text{(T-Let)} \\]

```rust
#![function!("algorithm-w/src/infer.rs", TypeInference::infer_let)]
```

Let expressions enable polymorphism through generalization. After inferring the bound expression's type, we generalize it by quantifying over type variables not constrained by the environment. This allows polymorphic usage in the let body.

### Literal Types

\\[ \\frac{}{Γ ⊢ n : \\text{Int}} \\text{(T-LitInt)} \\]

\\[ \\frac{}{Γ ⊢ b : \\text{Bool}} \\text{(T-LitBool)} \\]

```rust
#![function!("algorithm-w/src/infer.rs", TypeInference::infer_lit_int)]
```

```rust
#![function!("algorithm-w/src/infer.rs", TypeInference::infer_lit_bool)]
```

Literals have known types and require no constraint generation.

## Generalization and Instantiation

The generalization and instantiation mechanisms handle let-polymorphism, allowing variables bound in let expressions to be used with multiple different types.

### Understanding Generalization with Examples

Generalization turns concrete types into polymorphic type schemes. Consider this simple example:

```haskell
let id = \x -> x in (id 42, id true)
```

When we infer the type of `\x -> x`, we get something like `t0 → t0` where `t0` is a type variable. Since `t0` doesn't appear anywhere else in the environment, we can generalize it to `∀t0. t0 → t0`, making `id` polymorphic.

This is what allows `id` to be used with both `42` (type `Int`) and `true` (type `Bool`) in the same expression. Without generalization, the first use would fix `t0` to `Int`, making the second use fail.

```rust
#![function!("algorithm-w/src/infer.rs", TypeInference::generalize)]
```

Generalization identifies type variables that could be made polymorphic by checking which ones don't appear free in the current environment. If a type variable isn't constrained by anything else in scope, it's safe to quantify over it.

### Understanding Instantiation with Examples

Instantiation creates fresh monomorphic versions from polymorphic types. When we use a polymorphic function like our identity function, we need to create a fresh copy of its type for each use.

Consider this expression:

```haskell
let id = \x -> x in id id
```

Here we're applying the polymorphic identity function to itself. The first `id` gets instantiated to `(α → α) → (α → α)` while the second `id` gets instantiated to `α → α`. These different instantiations allow the application to type-check successfully.

```rust
#![function!("algorithm-w/src/infer.rs", TypeInference::instantiate)]
```

Instantiation replaces quantified type variables with fresh type variables. This ensures that each use of a polymorphic function gets its own independent type constraints, preventing interference between different call sites.

### Free Type Variables

Generalization depends on computing the free type variables in both individual types and entire environments. These operations identify which type variables could potentially be generalized versus those that are already constrained.

```rust
#![function!("algorithm-w/src/infer.rs", TypeInference::free_type_vars)]
```

The free type variables computation traverses type structures recursively, collecting all type variables that appear unbound. For compound types like arrows and tuples, it must traverse all subcomponents to ensure no variables are missed.

```rust
#![function!("algorithm-w/src/infer.rs", TypeInference::free_type_vars_env)]
```

Computing free variables across entire environments requires examining every type in the environment and taking the union of their free variables. This gives us the complete set of type variables that are constrained by the current context.

Our complete implementation fully supports polymorphic instantiation by generating fresh type variables for each quantified variable in a scheme when it is instantiated. This mechanism is what allows the identity function to work on integers in one context and booleans in another, as demonstrated by expressions like `let id = \x -> x in (id, id)` which produces the type `(t1 -> t1, t2 -> t2)` showing proper polymorphic instantiation.

## Error Handling and Inference Trees

Our implementation provides detailed error reporting and generates inference trees that show the step-by-step reasoning process.

```rust
#![struct!("algorithm-w/src/infer.rs", InferenceTree)]
```

These inference trees serve both debugging and educational purposes. They make explicit the implicit reasoning that Algorithm W performs, showing how type information flows through the program and how constraints get generated and solved.

The public interface provides both tree-generating and type-only versions of inference, supporting different use cases from interactive development to automated tooling.

```rust
#![function!("algorithm-w/src/infer.rs", infer_type_only)]
```

## Example Usage

To see Algorithm W in action, let's type-check a polymorphic function that demonstrates let-polymorphism and generalization:

```bash
$ cargo run -- "let const = \\x -> \\y -> x in const 42 true"
```

This produces the following clean output showing the complete inference process:

```
Parsed expression: let const = λx.λy.x in const 42 true

Type inference successful!
Final type: Int

Inference trace:
T-Let: {} ⊢ let const = λx.λy.x in const 42 true ⇒ => Int
  T-Abs: {} ⊢ λx.λy.x ⇒ => t0 → t1 → t0
    T-Abs: {x: t0} ⊢ λy.x ⇒ => t1 → t0
      T-Var: {x: t0, y: t1} ⊢ x ⇒ => t0
  T-App: {const: forall t0 t1. t0 → t1 → t0} ⊢ const 42 true ⇒ => Int
    T-App: {const: forall t0 t1. t0 → t1 → t0} ⊢ const 42 ⇒ => t5 → Int
      T-Var: {const: forall t0 t1. t0 → t1 → t0} ⊢ const ⇒ => t4 → t5 → t4
      T-Int: {const: forall t0 t1. t0 → t1 → t0} ⊢ 42 ⇒ => Int
      Unify-Arrow: t4 → t5 → t4 ~ Int → t3 => {t5 → Int/t3, Int/t4}
        Unify-Var: t4 ~ Int => {Int/t4}
        Unify-Var: t5 → Int ~ t3 => {t5 → Int/t3}
    T-Bool: {const: forall t0 t1. t0 → t1 → t0} ⊢ true ⇒ => Bool
    Unify-Arrow: t5 → Int ~ Bool → t2 => {Int/t2, Bool/t5}
      Unify-Var: t5 ~ Bool => {Bool/t5}
      Unify-Var: Int ~ t2 => {Int/t2}
```

This trace shows several key aspects of Algorithm W:

1. **Generalization**: The lambda `\x -> \y -> x` initially gets type `t0 → t1 → t0`, but when bound to `const` in the let-expression, it's generalized to `∀t0 t1. t0 → t1 → t0`.

2. **Instantiation**: When `const` is used in the application, it gets instantiated with fresh type variables `t4` and `t5`, allowing it to be used polymorphically.

3. **Unification**: The constraints from applying `const` to `42` and `true` get solved through unification, determining that `t4 = Int`, `t5 = Bool`, and the final result type is `Int`.

The final result is `Int`, showing that `const 42 true` correctly returns the first argument (42) regardless of the second argument's type (true).

These interface functions demonstrate how Algorithm W can be embedded into larger systems. The tree-generating version supports educational tools and debuggers, while the type-only version provides the minimal interface needed for type checking during compilation.
