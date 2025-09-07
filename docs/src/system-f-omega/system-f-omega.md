# System Fω

System Fω stands as one of the most  type systems in the theoretical foundations of programming languages, combining parametric polymorphism with higher-kinded types to enable powerful abstraction mechanisms. This system forms the theoretical backbone for modern functional programming languages like Haskell and provides the expressive power needed for advanced programming patterns including functors, monads, and generic programming.

To understand System Fω's significance, we must first examine its place within the broader landscape of type systems through the systematic classification known as the **lambda cube**.

## The Lambda Cube

The lambda cube, introduced by Henk Barendregt, provides a systematic way to understand how different type systems relate to each other by considering three independent dimensions of abstraction. Each dimension of the cube corresponds to a different kind of dependency between terms and types, where "dependency" refers to the capacity of a term or type to bind a term or type.

The three orthogonal axes of the lambda cube correspond to:

**\\(\to\\)-axis (Types depending on Terms)**: Enables dependent types where the structure of types can depend on the values of terms. This allows types like `Vector n` where the type carries information about term-level values.

**\\(\uparrow\\)-axis (Terms depending on Types)**: Enables polymorphism where terms can abstract over and depend on type parameters. This allows functions like `identity : ∀α. α \\to α` that work uniformly across all types.

**\\(\nearrow\\)-axis (Types depending on Types)**: Enables type operators where types can abstract over and depend on other types. This allows type constructors like `Maybe : * \\to *` that take types as arguments and produce new types.

The eight vertices of the cube emerge from the different ways to combine these three dimensions of dependency. Each vertex represents a different typed system, obtained by enabling or disabling each form of abstraction:

**Simply Typed Lambda Calculus (λ→)**: The foundation of the cube, supporting only term abstraction. Functions can abstract over terms (`λx:τ. e`) but types remain fixed and simple.

**System F (λ2)**: Adds polymorphism through type abstraction, enabling terms to abstract over types (`Λα. e`). This introduces parametric polymorphism where functions like identity can work uniformly across all types.

**System Fω (λω)**: Introduces higher-kinded types by adding type operators, allowing types to abstract over types (`λα:κ. τ`). This enables abstraction over type constructors like `Maybe` and `List`.

**Lambda P (λP)**: Adds dependent types where types can depend on terms. This allows types like `Vector n` where the length `n` is a term-level value, enabling precise specifications of data structure properties.

**System Fω (λ2ω)**: Combines polymorphism with higher-kinded types, enabling both term abstraction over types and type abstraction over types. This is our target system, providing the expressiveness needed for modern functional programming.

**System Fω-P (λωP)**: Combines higher-kinded types with dependent types, allowing  type-level computation that can depend on term-level values.

**System FP (λ2P)**: Combines polymorphism with dependent types, enabling functions that are parametric over both types and values while allowing types to depend on those values.

**Calculus of Constructions (λ2ωP)**: The most expressive corner, combining all three forms of abstraction. This system underlies proof assistants like Coq and enables types that can express arbitrary logical propositions.

The lambda cube demonstrates that these eight systems form a natural hierarchy, with each system being a conservative extension of those below it in the ordering. System Fω sits at a sweet spot, providing significant expressive power while maintaining decidable type checking and relatively straightforward implementation techniques.

Where System F introduced universal quantification over types (`∀α. τ`), System Fω extends this to quantification over type constructors of arbitrary kinds. This enables us to write functions that are polymorphic not just over types like `Int` or `Bool`, but over type constructors like `Maybe` or `List`, and even higher-order type constructors that take multiple type arguments.

The type system is stratified by **kinds**, which classify types just as types classify terms:

\\[ \begin{align*}
\text{kinds} \quad \kappa &::= \star \mid \kappa_1 \to \kappa_2 \\\\
\text{types} \quad \tau &::= \alpha \mid \tau_1 \to \tau_2 \mid \forall \alpha:\kappa. \tau \mid \tau_1 \tau_2 \mid \lambda \alpha:\kappa. \tau \\\\
\text{terms} \quad e &::= x \mid \lambda x:\tau. e \mid e_1 e_2 \mid \Lambda \alpha:\kappa. e \mid e[\tau]
\end{align*} \\]

This hierarchy extends naturally upward: just as we need types to classify terms and kinds to classify types, we could in principle introduce **sorts** to classify kinds.

While System Fω stops at the kind level for practical reasons, the theoretical framework suggests an infinite tower: terms have types, types have kinds, kinds have sorts, sorts have supersorts, and so on. This infinite hierarchy, known as the **cumulative hierarchy** in type theory, reflects the fundamental principle that every mathematical object must be classified by something at a higher level of abstraction. The aleph notation acknowledges this infinite ascent while recognizing that practical type systems must terminate the hierarchy at some finite level. More on this when we get to dependent types later!

## Higher-Kinded Types

The key innovation of System Fω is the kind system. While ordinary types like `Int` and `Bool` have kind `*` (pronounced "star"), type constructors have function kinds:

- `Maybe` has kind `* -> *` (takes a type, returns a type)
- `Either` has kind `* -> * -> *` (takes two types, returns a type)
- A hypothetical `StateT` might have kind `* -> (* -> *) -> * -> *`

This stratification enables precise reasoning about type-level functions while maintaining decidable type checking.

Our implementation demonstrates these concepts through a rich surface language that compiles to a core System Fω calculus. The surface syntax provides familiar algebraic data types and pattern matching:

```haskell
data Maybe a = Nothing | Just a
data Either a b = Left a | Right b
data List a = Nil | Cons a (List a)
```

These declarations create type constructors of appropriate kinds. `Maybe` becomes a type-level function that, when applied to a type like `Int`, produces the concrete type `Maybe Int`.

## Type-Level Computation

System Fω supports type-level computation through type application and type abstraction. While our implementation focuses on the foundational mechanisms, the theoretical system allows arbitrary computation at the type level:

* **Type Application**: `F τ` applies type constructor `F` to type `τ`
* **Type Abstraction**: `λα:κ. τ` creates a type-level function

This enables  type-level programming, though our implementation focuses on the essential features needed for practical programming language design.

## Polymorphic Data Structures

The combination of universal quantification and higher-kinded types enables elegant expression of polymorphic data structures and functions:

```haskell
map :: forall a b. (a -> b) -> List a -> List b
foldRight :: forall a b. (a -> b -> b) -> b -> List a -> b
```

These functions work uniformly across all types, demonstrating the power of parametric polymorphism in System Fω.

## Implementation Architecture

Our System Fω implementation consists of several key components working together:

* **Surface Language**: A user-friendly syntax with algebraic data types, pattern matching, and type inference
* **Core Language**: An explicit System Fω calculus with kinds, type abstractions, and applications
* **Elaboration**: Translation from surface to core, inserting implicit type arguments and abstractions
* **Type Inference**: A bidirectional algorithm based on the DK worklist approach
* **Kind Inference**: Automatic inference of kinds for type constructors and type expressions

The implementation demonstrates that  type systems can be made practical through careful algorithm design and implementation techniques.

Our implementation uses bidirectional type checking, which splits the problem into two complementary modes:

* **Synthesis (⇒)**: Given an expression, determine its type
* **Checking (⇐)**: Given an expression and expected type, verify compatibility

This approach handles the complexity of higher-rank polymorphism and type applications while maintaining decidability and providing good error messages.
