# Language Design

Our System Fω implementation employs a two-layer architecture that separates user-facing syntax from the internal representation used by the type checker. This design pattern, common in  compilers, allows us to provide an ergonomic programming experience while maintaining a clean theoretical foundation for type checking algorithms.

The **surface language** offers familiar syntax with algebraic data types, pattern matching, and implicit type inference. The **core language** provides an explicit representation of System Fω with kinds, type abstractions, and applications. Translation between these layers handles the complex process of inserting implicit type arguments and managing the  type-level computations that System Fω enables.

## The Haskell Hairball

Before diving into our clean System Fω design, we must acknowledge the elephant in the room: Haskell, the language that wore the hairshirt for two decades and somehow convinced a generation of programmers that this constituted virtue. Our implementation deliberately avoids the accumulated cruft that has transformed what began as an elegant research language into something resembling a Lovecraftian nightmare of interacting language extensions.

Haskell represents a fascinating case study in how good intentions, academic enthusiasm, and the sunk cost fallacy can combine to create somethign is both a beautiful and a trainwreck at the same time. Consider the design decisions that seemed reasonable at the time but now serve as warnings to future language designers:

* **Call-by-need evaluation**: Lazy evaluation sounds theoretically elegant until you discover that reasoning about space and time complexity requires divination skills, and debugging memory leaks involves consulting tea leaves about thunk accumulation patterns

* **LARPing dependent types**: Rather than implementing actual dependent types, Haskell developed an increasingly baroque system of type-level programming that lets you pretend you have dependent types while maintaining all the complexity and none of the theoretical guarantees

* **Language extension proliferation**: What started as a few modest extensions has metastasized into a catalog of over 200 language pragmas, creating not one language but a thousand mutually incompatible dialects that share only syntax

* **Infinite RAM Requirements**: The GHC compiler requires approximately the computational resources of a small Dyson swarm to bootstrap itself, making it effectively impossible to port to new architectures without access to industrial-scale computing infrastructure

Haskell, while historically important and technically very interesting, is an excellent guide for how *not* to design a language.

## Surface Language Syntax

The surface language provides a Haskell-like syntax that programmers can work with naturally. Users write programs without explicit type abstractions or applications, and the compiler inserts these automatically during elaboration to the core language.

### Data Type Declarations

```rust
#![enum!("system-f-omega/src/surface.rs", Declaration)]
```

Algebraic data types form the foundation of our surface language. These declarations create both type constructors and value constructors:

```haskell
data Bool = True | False;
data Maybe a = Nothing | Just a;
data Either a b = Left a | Right b;
data List a = Nil | Cons a (List a);
```

Each data declaration introduces:
- A **type constructor** (like `Maybe` or `List`) that can be applied to type arguments
- **Value constructors** (like `Nothing`, `Just`, `Nil`, `Cons`) for creating values of the type
- Implicit **kind information** determined by the number and usage of type parameters

The surface language allows natural type parameter syntax where `data List a` automatically implies that `List` has kind `* -> *`.

### Type Signatures and Schemes

```rust
#![struct!("system-f-omega/src/surface.rs", TypeScheme)]
```

Type schemes provide explicit polymorphic type signatures using familiar quantifier syntax:

```haskell
identity :: a -> a;
const :: a -> b -> a;
map :: (a -> b) -> List a -> List b;
```

The surface language uses implicit quantification - any free type variables in a type signature are automatically universally quantified. This provides the convenience of languages like Haskell while maintaining the theoretical precision of System Fω.

### Surface Types

```rust
#![enum!("system-f-omega/src/surface.rs", Type)]
```

The surface type system includes:

- **Type variables** (`a`, `b`) for generic parameters
- **Type constructors** (`Int`, `Bool`, `List`) for concrete and parameterized types
- **Function types** (`a -> b`) with right-associative arrow notation
- **Type application** (`List a`, `Maybe Int`) for applying type constructors

Notably absent from the surface language are explicit type abstractions, type applications to terms, and kind annotations. These are inferred and inserted automatically during elaboration.

### Expression Language

```rust
#![enum!("system-f-omega/src/surface.rs", Expr)]
```

The surface expression language supports:

**Variables and Literals**: Standard identifiers and numeric/boolean constants
**Function Application**: `f x` applies function `f` to argument `x`
**Lambda Abstractions**: `\x -> e` creates anonymous functions
**Pattern Matching**: `match e { p1 -> e1; p2 -> e2; }` for case analysis
**Constructor Application**: `Just 42`, `Cons x xs` for building data structures

The surface language omits explicit type abstractions (`Λα. e`) and type applications (`e [τ]`). These System Fω constructs are handled automatically by the elaboration process.

## Core Language Representation

The core language provides an explicit encoding of System Fω with full type-level computation capabilities. This representation makes type checking tractable by exposing all implicit operations from the surface language.

### Core Types and Kinds

```rust
#![enum!("system-f-omega/src/core.rs", Kind)]
```

The kind system classifies types hierarchically:
- **`Star`** (`*`) for ordinary types like `Int`, `Bool`, `List Int`
- **`Arrow(k1, k2)`** for type constructors like `Maybe : * -> *` or `Either : * -> * -> *`

```rust
#![enum!("system-f-omega/src/core.rs", CoreType)]
```

Core types include all the expressive power of System Fω:

**Type Variables**: Both ordinary (`Var`) and existential (`ETVar`) variables for unification
**Type Constructors**: Built-in types (`Con`) like `Int`, `Bool`
**Function Types**: Explicit arrow types (`Arrow`)
**Universal Quantification**: `Forall` types with kind-annotated bound variables
**Type Application**: `App` for applying type constructors to arguments
**Type Abstraction**: `TAbs` for creating type-level functions

The core representation makes explicit all type-level computation that remains implicit in the surface language.

### Core Terms

```rust
#![enum!("system-f-omega/src/core.rs", CoreTerm)]
```

Core terms expose the full System Fω term language:

**Variables and Literals**: Direct correspondence with surface language
**Function Abstraction/Application**: Explicitly typed lambda calculus
**Type Abstraction**: `TyAbs` for creating polymorphic functions
**Type Application**: `TyApp` for instantiating polymorphic functions
**Data Constructors**: `Con` for algebraic data type constructors
**Pattern Matching**: `Match` with explicit constructor patterns

Every implicit type operation from the surface language becomes explicit in the core representation.

## Pattern Matching and Case Analysis

Pattern matching provides the fundamental mechanism for deconstructing algebraic data types and extracting their constituent values. Our implementation supports comprehensive pattern matching that integrates seamlessly with the type system, ensuring that all pattern analyses are both exhaustive and type-safe.

The pattern matching construct `match e { p1 -> e1; p2 -> e2; }` performs case analysis on the scrutinee expression `e`, attempting to match it against each pattern in sequence. When a pattern matches, the corresponding branch expression executes with pattern variables bound to the extracted values.

### Constructor Patterns and Variable Binding

```rust
#![enum!("system-f-omega/src/surface.rs", Pattern)]
```

Constructor patterns decompose algebraic data types by matching against specific constructors and binding their arguments to pattern variables. The pattern `Cons x xs` matches values constructed with `Cons`, binding the first argument to `x` and the second to `xs`. This binding mechanism provides type-safe access to the components of structured data.

Variable patterns like `x` match any value and bind the entire matched value to the variable name. The pattern matching compiler ensures that each variable binding receives the appropriate type based on the context in which the pattern appears.

Wildcard patterns represented by `_` match any value without creating bindings, useful for ignoring components that are not needed in the branch expression. The type checker verifies that wildcard patterns are used consistently with the expected type structure.

### Exhaustiveness and Type Safety

The pattern matching implementation enforces exhaustiveness, requiring that pattern sets cover all possible values of the matched type. For algebraic data types, this means providing patterns for every constructor defined in the type declaration. The compiler rejects programs with non-exhaustive patterns, preventing runtime errors that could occur when unhandled cases arise.

Type safety extends through pattern matching by ensuring that pattern variables receive types consistent with the constructor arguments they represent. When matching `Just x` against a `Maybe Int`, the variable `x` automatically receives type `Int`, eliminating the need for explicit type annotations or runtime type checks.

### Nested Patterns and Deep Matching

Patterns can nest arbitrarily deeply, enabling  decomposition of complex data structures in single pattern matches. The pattern `Cons (Just x) xs` simultaneously matches the outer list structure and the inner `Maybe` type, binding both the unwrapped value `x` and the remaining list `xs` in a single operation.

Nested pattern matching interacts correctly with polymorphism, maintaining type relationships across multiple levels of structure. The type checker propagates type information through nested patterns, ensuring that all bindings receive their most general types while maintaining compatibility with the overall pattern context.

## Algebraic Data Types

Algebraic data types provide the foundation for structured data in our System Fω implementation, combining sum types (disjoint unions) and product types (tuples and records) into a unified framework that supports both data abstraction and generic programming.

The data declaration syntax enables concise specification of complex type structures while automatically deriving the associated constructors, destructors, and type information needed for pattern matching and type checking.

### Sum Types and Tagged Unions

Sum types represent choices between alternative data representations, with each alternative identified by a unique constructor tag. The declaration `data Either a b = Left a | Right b` creates a sum type with two alternatives, each carrying a value of a different type.

Sum types enable type-safe representation of optional values, error conditions, and other scenarios where data can take one of several forms. The type `Maybe a = Nothing | Just a` encapsulates the common pattern of values that might be absent, replacing null pointer patterns with type-safe alternatives.

Each constructor in a sum type creates values that are distinguishable through pattern matching, enabling exhaustive case analysis that the type checker can verify statically. The tag information embedded in sum type values allows pattern matching to dispatch correctly to the appropriate branch without runtime type inspection.

### Product Types and Data Aggregation

Product types combine multiple values into single composite structures, with each component accessible through pattern matching or projection operations. Constructor syntax like `Cons a (List a)` creates product types where each constructor argument represents a field in the resulting structure.

Product types support both named and positional field access through pattern matching, providing flexibility in how composite data gets decomposed. The pattern `Cons head tail` extracts both components of a list cell, binding them to appropriately typed variables for use in the branch expression.

Tuples represent anonymous product types where component ordering determines access patterns. While our surface language does not include explicit tuple syntax, the pattern matching mechanism supports tuple-like destructuring of constructor arguments.

### Recursive Types and Inductive Data Structures

Recursive type definitions enable the construction of arbitrarily large data structures through self-reference in constructor arguments. The declaration `data List a = Nil | Cons a (List a)` defines lists inductively, with the base case `Nil` and the recursive case `Cons` that references the type being defined.

Recursive types interact correctly with polymorphism, enabling the definition of generic container types that work uniformly across all element types. The list type `List a` demonstrates how recursive structure combines with parametric polymorphism to create flexible, reusable data abstractions.

Inductive types support well-founded recursion through pattern matching, enabling terminating recursive functions that process arbitrarily large data structures. The pattern matching compiler ensures that recursive calls operate on structurally smaller arguments, supporting termination analysis and optimization.

### Kind Inference and Type Constructor Classification

Data type declarations automatically infer appropriate kinds for the defined type constructors based on their parameter structure and usage patterns. Simple types like `Bool` receive kind `*`, while parameterized types like `Maybe` receive kinds of the form `* -> *`.

Higher-kinded types emerge naturally from data declarations with multiple parameters or higher-order structure. The type `Either` receives kind `* -> * -> *`, indicating a type constructor that requires two type arguments to produce a complete type.

Kind inference propagates through type expressions, ensuring that type applications in data constructors receive appropriate kind annotations for use in the core language representation. This automatic kind inference eliminates the need for explicit kind annotations while maintaining the precision required for System Fω's  type-level computation.

## Elaboration Process

Elaboration forms the critical bridge between the user-friendly surface language and the theoretically precise core language, transforming high-level programming constructs into explicit System Fω representations that enable sound type checking and compilation. This translation process handles the complex task of inferring and inserting all implicit type-level operations that the surface language deliberately omits for conciseness and programmer convenience.

The elaboration algorithm operates through multiple interdependent phases that work together to produce well-typed core language programs. Each phase builds upon the results of previous phases, creating a pipeline that systematically transforms surface constructs into their explicit core equivalents while maintaining type safety and semantic preservation.

### Kind Inference and Type Constructor Analysis

Kind inference represents one of the most  aspects of elaboration, requiring analysis of type constructor usage patterns to determine their proper classification in the kind hierarchy. The process begins by analyzing type parameters in data declarations to determine the kinds of type constructors being defined. A simple data type like `Bool` with no parameters receives kind \\( \\star \\), indicating it represents a complete type that can classify terms.

Parameterized data types require more complex analysis to determine their proper kinds. The declaration `data Maybe a = Nothing | Just a` reveals that `Maybe` is a type constructor that takes one type argument, yielding kind \\( \\star \\to \\star \\). Multi-parameter types like `Either a b` receive kinds of the form \\( \\star \\to \\star \\to \\star \\), reflecting their need for multiple type arguments before producing complete types.

The kind inference algorithm propagates kind constraints through type applications and signatures, ensuring consistency across the entire program. When a type constructor appears in a type application like `Maybe Int`, the algorithm verifies that the argument `Int` has kind \\( \\star \\) to match the expected parameter kind of `Maybe`. This constraint propagation catches kind errors early in the elaboration process, preventing malformed types from reaching the core language.

The final step inserts implicit kind abstractions in the core representation, making explicit the kind-level lambda abstractions that correspond to parameterized type constructors. The surface type `Maybe` becomes a kind-level function \\( \\lambda\\alpha :: \\star. \\text{Maybe}\\, \\alpha \\) in the core language, exposing the type-level computation that remains hidden in the surface syntax.

### Type Inference and Polymorphic Instantiation

Surface programs deliberately omit many type-level operations to maintain readability and reduce annotation burden, requiring the elaboration process to infer and insert these operations automatically. The most complex aspect involves handling polymorphic functions, which use implicit universal quantification in the surface language but require explicit type abstractions and applications in the core.

Type abstraction insertion analyzes function definitions to identify polymorphic variables that must be abstracted at the type level. A surface function like `identity x = x` with inferred type `a -> a` becomes a core expression \\( \\Lambda\\alpha :: \\star. \\lambda x : \\alpha. x \\), making explicit the type-level abstraction over the polymorphic variable `a`. This transformation ensures that the core representation captures the full generality of polymorphic functions.

Type application insertion occurs at call sites of polymorphic functions, where the surface language relies on type inference to determine appropriate instantiations. When a polymorphic function like `identity` is applied to a specific argument like `42`, the elaboration process inserts the type application \\( \\text{identity}[\\text{Int}]\\, 42 \\), making explicit the instantiation of the type parameter with `Int`.

The generation of existential variables handles unknown types that arise during inference, creating placeholders that the constraint solver can resolve later. When the elaboration process encounters expressions whose types cannot be determined immediately, it generates fresh existential variables and creates constraints that guide the solver toward appropriate instantiations.

### Pattern Compilation and Constructor Analysis

Pattern matching in the surface language undergoes substantial transformation during elaboration, converting high-level pattern matching constructs into explicit case analysis that the core language can represent directly. This compilation process must handle the complex interactions between pattern structure, type information, and variable binding that make pattern matching both powerful and type-safe.

Constructor pattern analysis examines each pattern to determine the type of the scrutinee expression and the types of bound variables. A pattern like `Cons x xs` matching against `List Int` reveals that `x` has type `Int` and `xs` has type `List Int`. This type information gets embedded in the core representation to guide type checking and code generation.

The generation of core match expressions creates explicit case analysis constructs that enumerate all possible constructor alternatives. Surface pattern matches become core expressions of the form \\( \\text{match}\\, e\\, \\{ \\text{C}_1\\, x_1 \\to e_1; \\ldots; \\text{C}_n\\, x_n \\to e_n \\} \\), with each alternative explicitly typed and all constructor possibilities accounted for.

Pattern variable binding requires careful scope management to ensure that variables bound in patterns are available with the correct types in branch expressions. The elaboration process maintains binding contexts that track the types of pattern variables and ensures that core expressions correctly reference these bindings with appropriate type information.

Exhaustiveness checking during pattern compilation verifies that pattern sets cover all possible constructor alternatives, preventing runtime errors from unhandled cases. The elaboration process analyzes constructor declarations to determine the complete set of alternatives and reports errors when patterns are non-exhaustive, maintaining the type safety guarantees that System Fω provides.

## Design Rationale

The two-layer architecture of our System Fω implementation represents a carefully considered approach to balancing theoretical precision with practical usability, addressing fundamental tensions that arise when building advanced type systems for real-world programming. This design philosophy emerges from the recognition that pure System Fω, while theoretically elegant, imposes significant annotation burden that makes it impractical for everyday programming tasks.

### Programmer Productivity and Cognitive Load Management

The surface language prioritizes programmer productivity by eliminating the explicit type-level operations that System Fω requires while preserving all the expressive power of the underlying type system. Programmers can write natural, intuitive code using familiar algebraic data types and pattern matching without being forced to understand or manipulate higher-kinded types, type applications, or kind annotations directly.

This approach recognizes that cognitive load represents a scarce resource in software development. By hiding the complexity of type-level computation behind a clean surface syntax, we enable programmers to focus on problem-solving rather than wrestling with the mechanical details of type system operation. The surface language provides enough abstraction that polymorphic programming feels natural and obvious, even though the underlying elaboration process involves  type inference and constraint solving.

The implicit quantification system exemplifies this philosophy. Where pure System Fω requires explicit type abstractions like \\( \\Lambda\\alpha :: \\star. \\lambda x : \\alpha. x \\), our surface language allows the simple definition `identity x = x` with automatic inference of the polymorphic type `a -> a`. This transformation eliminates tedious annotation while preserving full generality and type safety.

### Type Safety and Theoretical Soundness

The core language ensures that all the theoretical guarantees of System Fω remain intact by making every type operation explicit and checkable. This explicit representation enables rigorous type checking algorithms that can verify program correctness with mathematical precision, preventing the subtle errors that can arise when implicit operations are handled incorrectly.

By elaborating surface programs to core representations, we gain access to the full theoretical machinery of System Fω, including decidable type checking, principal types, and strong normalization. The core language serves as a certificate of correctness, providing concrete evidence that surface programs satisfy all the constraints of the type system.

The explicit nature of core representations also enables advanced optimizations and program transformations that rely on precise type information. Code generators can exploit type-level information to produce efficient implementations, while program analysis tools can reason about program behavior with greater precision than would be possible with surface-level representations alone.

### Modularity and Language Evolution

The separation between surface and core languages provides crucial modularity that enables independent evolution of user-facing features and theoretical foundations. Surface language features can be added, modified, or removed without affecting the core type checking algorithms, provided the elaboration process can translate them to appropriate core representations.

This modularity proves essential for language experimentation and extension. New surface constructs like syntactic sugar, alternative pattern matching styles, or domain-specific language features can be implemented entirely through elaboration without requiring changes to the type checker or core language semantics. This flexibility enables rapid prototyping of language features while maintaining implementation stability.

The core language can similarly evolve independently, with improvements to type checking algorithms, optimization passes, or code generation strategies affecting all surface programs automatically through the existing elaboration interface. This separation enables focused development where surface language designers can concentrate on usability while type theorists can focus on correctness and efficiency.

### Debugging and Developer Understanding

The explicit core representations provide invaluable insight into type checker behavior, enabling developers to understand how their programs are interpreted by the type system. When type errors occur, the core representation shows exactly which type-level operations failed and why, providing much more precise diagnostic information than surface-level error reporting alone could offer.

This debugging capability extends to performance analysis, where core representations reveal the type-level computation overhead imposed by different programming patterns. Developers can examine core representations to understand which surface constructs generate expensive type-level operations and adjust their programming style accordingly.

The elaboration process itself serves as an educational tool, demonstrating how high-level programming constructs decompose into fundamental type theoretic operations. Students learning advanced type systems can examine the core representations of their programs to develop intuition about how polymorphism, higher-kinded types, and type-level computation actually work.

### Balancing Expressiveness with Accessibility

Our design demonstrates that advanced type systems can be made accessible without sacrificing their theoretical foundations. The surface language captures the essential patterns that programmers want to express - polymorphic functions, generic data structures, type-safe pattern matching - while the core language ensures that these patterns have rigorous foundations in type theory.

This balance addresses a fundamental challenge in programming language design: how to provide the benefits of advanced type systems without requiring all programmers to become experts in type theory. By carefully choosing which aspects to make implicit and which to keep explicit, we create a system that scales from beginning programmers writing simple functions to expert developers implementing complex generic libraries.

The success of this approach validates the broader principle that programming language design should prioritize human factors alongside theoretical considerations. Technical elegance and mathematical precision remain essential, but they must be balanced against the practical realities of software development where programmer time, cognitive load, and learning curves represent real constraints that affect the ultimate utility of programming languages.

Through this two-layer architecture, we achieve a System Fω implementation that provides the full power of higher-kinded polymorphism while remaining approachable for programmers who simply want to write correct, generic, type-safe code without becoming experts in the theoretical foundations that make such code possible.
