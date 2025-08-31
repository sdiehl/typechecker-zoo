# Type System

Our Calculus of Constructions implementation centers around a  type system that unifies terms, types, and kinds into a single syntactic framework. The implementation demonstrates how dependent types, universe polymorphism, and definitional equality work together to create a practical dependently-typed programming language.

## AST Design and Term Language

The core of our implementation lies in the unified term language that represents all syntactic categories within a single AST structure. This design reflects the CoC principle that terms, types, and kinds are all inhabitants of the same computational universe.

```rust
#![enum!("coc/src/ast.rs", Term)]
```

Our `Term` enum demonstrates the unification principle by using the same constructors to represent functions (`Abs`), function types (`Pi`), type constructors, and logical propositions. The same application constructor (`App`) represents both function application and type application, eliminating the artificial boundaries present in stratified type systems.

Variables (`Var`) and constants (`Const`) form the basic building blocks of our term language. Variables represent both term-level bindings and type-level bindings, while constants refer to defined names in the global context. The implementation treats both categories uniformly, enabling the same binding mechanisms to work across all abstraction levels.

### Function Types and Lambda Abstractions

The `Pi` constructor represents dependent product types, generalizing both simple function types and universal quantification. When the bound variable appears in the body type, we obtain dependency; when it does not appear, the Pi-type reduces to a simple function type.

```rust
// Pi(variable_name, domain_type, codomain_type, is_implicit)
Pi("x".to_string(), Box::new(nat_type), Box::new(vec_type), false)
```

The boolean flag indicates whether the parameter should be treated as implicit, enabling our implementation to support both explicit and implicit argument passing. Lambda abstractions (`Abs`) create inhabitants of Pi-types, with the same constructor serving for both computational functions and proof terms.

### Pattern Matching and Inductive Elimination

```rust
#![struct!("coc/src/ast.rs", MatchArm)]
```

Our implementation includes comprehensive pattern matching through the `Match` constructor, which enables elimination of inductive types. Each match arm specifies a constructor pattern and the corresponding elimination term, providing the computational content needed for inductive reasoning.

The pattern matching implementation supports nested patterns and variable bindings, enabling  destructions of complex inductive data structures while maintaining type safety through exhaustiveness checking.

## Universe System Implementation

The implementation includes a comprehensive universe system that prevents logical paradoxes while enabling flexible type-level computation. Our universe design supports both concrete levels and polymorphic universe variables.

```rust
#![enum!("coc/src/ast.rs", Universe)]
```

### Universe Levels and Arithmetic

The universe system supports multiple forms of universe expressions that enable both concrete level specifications and polymorphic universe computations. Concrete levels represented by `Const(n)` specify particular universe levels like `Type 0`, `Type 1`, and so forth, forming the foundation of the universe hierarchy and providing definite homes for specific types. Universe variables through `ScopedVar` enable universe polymorphism by allowing definitions to be parameterized over universe levels, permitting the same definition to work at multiple universe levels simultaneously. Level arithmetic operations using `Add` enable universe level computations such as `u + 1`, supporting common patterns where a type constructor must live at a universe level one higher than its parameter. Maximum operations provided by `Max` and `IMax` compute universe level selections that choose the higher of two levels, with `IMax` implementing the impredicative maximum used in proof-relevant contexts where the universe level computation must respect the logical structure of the proof.

### Universe Constraint Solving

```rust
#![enum!("coc/src/ast.rs", UniverseConstraint)]
```

Our implementation includes a dedicated universe constraint solver that handles the complex relationships between universe levels. The solver maintains a constraint graph and applies  algorithms to determine consistent universe level assignments. The constraint solver manages level equality constraints that require two universe levels to be identical, arising from type equality requirements in dependent contexts where definitional equality demands universe level consistency. Level ordering constraints require one universe level to be strictly less than another, arising from the predicativity requirements of the type system that prevent logical paradoxes by maintaining a strict hierarchy of type universes. Arithmetic constraints involving universe level computations enable flexible universe level expressions while maintaining consistency, allowing complex universe polymorphic definitions to specify their universe requirements precisely through level arithmetic expressions that the solver can resolve to concrete level assignments.

## Definitional Equality and Normalization

Our type checker implements definitional equality through a comprehensive normalization algorithm that handles β-reduction, η-expansion, and definitional unfolding of constant definitions.

```rust
#![function!("coc/src/typecheck.rs", TypeChecker::normalize)]
```

The normalization algorithm implements β-reduction for function applications, handling both computational reductions and type-level computations. When a lambda abstraction is applied to an argument, the implementation performs substitution while carefully managing variable capture and scope.

\\[ (λx : Nat. x + 1) \\ 5  ⟹  5 + 1  ⟹  6 \\]

The implementation extends β-reduction to handle dependent type computations, where type-level functions can be applied to produce new types through computation.

### Eta Conversion

Eta conversion ensures that functions are equal to their eta-expanded forms, providing extensional equality for function types. The implementation applies η-expansion during normalization to ensure that definitionally equal terms are recognized as such.

\\[ λx : A. f \\ x  ≡  f \\text{ (when } x \\notin \\text{fv}(f)\\text{)} \\]

### Let-Expansion and Definition Unfolding

The implementation handles `let` bindings through expansion, replacing let-bound variables with their definitions during normalization. This approach ensures that local definitions do not interfere with definitional equality while providing the computational content needed for type checking.

Definition unfolding enables the type checker to access the computational content of defined constants, allowing definitional equality to work across module boundaries and enabling powerful abstraction mechanisms.

## Type Checking Algorithm

Our implementation employs bidirectional type checking that splits the type checking problem into synthesis and checking modes. This approach handles the complexity of dependent types while maintaining decidability and providing informative error messages.

```rust
#![function!("coc/src/typecheck.rs", TypeChecker::infer)]
```

### Type Synthesis

The synthesis algorithm determines the type of a term by analyzing its structure and propagating type information through the syntax tree. Variable lookup operates by consulting the typing context, which maintains bindings for both term variables and type variables, ensuring that each variable reference corresponds to a valid binding in the current scope. Function application typing proceeds by synthesizing the function type, ensuring it forms a Pi-type, and checking that the argument type matches the domain specification, with careful handling of dependent types where the codomain may depend on the specific argument value. Pi-type formation verification ensures both the domain and codomain are well-typed, with the codomain checked in a context extended with the bound variable to properly handle the dependency relationship between the domain and codomain types.

```rust
#![function!("coc/src/typecheck.rs", TypeChecker::check)]
```

### Type Checking Mode

The checking algorithm verifies that a term has an expected type by comparing the term structure with the type structure. This mode often provides better error messages and more efficient checking for terms with complex dependent types. Lambda checking operates by verifying lambda abstractions against Pi-types, ensuring that the body has the correct type in the extended context where the lambda parameter is properly bound. When synthesis and checking produce different results, the algorithm falls back to definitional equality checking, normalizing both the synthesized and expected types to their canonical forms and comparing the results, enabling the type checker to recognize when terms are definitionally equal despite having different syntactic representations.

### Context Management

```rust
#![struct!("coc/src/context.rs", Context)]
```

The typing context maintains bindings for variables, definitions, axioms, and constructors. Our implementation uses a  context structure that enables efficient lookup while supporting the complex scoping rules required for dependent types. Variable binding operations add new variable bindings with their types, enabling proper scoping in lambda abstractions and Pi-types where the bound variable may appear in the type of subsequent bindings. Definition extension capabilities allow adding new constant definitions with their types and bodies, enabling modular development and abstraction mechanisms that support large-scale program organization. Constructor registration functionality registers inductive type constructors with their types, enabling pattern matching and inductive reasoning that respects the structural properties of the data types and maintains type safety throughout elimination operations.

## Implicit Arguments and Elaboration

Our implementation includes comprehensive support for implicit arguments that are automatically inferred by the type checker. This feature bridges the gap between the fully explicit internal representation and the more convenient surface syntax.

```rust
#![function!("coc/src/typecheck.rs", TypeChecker::elaborate_implicit_parameters)]
```

### Implicit Argument Insertion

The type checker automatically inserts implicit arguments when encountering function applications where the function type has implicit parameters. Meta-variable generation creates fresh meta-variables to represent unknown implicit arguments, ensuring that each implicit parameter has a unique placeholder that can be resolved through subsequent constraint solving. Constraint collection gathers relationships that connect meta-variables to known type information, building a system of equations that captures the interdependencies between implicit arguments and the explicitly provided terms. Constraint solving employs the dedicated constraint solver to determine concrete values for meta-variables, using unification algorithms and type-directed search to find solutions that satisfy all accumulated constraints while respecting the type structure of the program.

### Meta-variable Resolution

```rust
#![struct!("coc/src/solver.rs", MetaInfo)]
```

Meta-variables represent unknown terms that get resolved through unification and constraint solving. Our implementation tracks meta-variable dependencies and applies  solving algorithms to determine unique solutions when possible.

## Takeaways

The Calculus of Constructions represents a unique convergence of computational and logical capabilities that distinguishes it from other type systems. The fundamental characteristic that sets CoC apart is its unification of terms, types, and kinds within a single syntactic framework, eliminating the artificial stratification found in traditional type systems. This unification enables unprecedented expressiveness where types can depend on computational values, creating a system where mathematical specifications and their implementations coexist naturally within the same linguistic framework.

The definitional equality mechanism provides computational content to the type system through β-reduction, η-conversion, and definition unfolding, creating a rich equational theory that recognizes when different syntactic expressions represent the same computational object. This equality extends beyond simple syntactic matching to include semantic equivalence, enabling the type checker to recognize mathematical identities and computational transformations automatically. The normalization algorithm ensures that definitional equality checking remains decidable while providing the computational power needed for type-level computations.
