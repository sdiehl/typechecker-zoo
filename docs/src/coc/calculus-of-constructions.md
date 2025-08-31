# Calculus of Constructions

The Calculus of Constructions (or **CoC** for short) represents the pinnacle of the lambda cube, occupying the most expressive corner where all three dimensions of abstraction converge. This system unifies terms, types, and kinds into a single syntactic framework, eliminating the artificial boundaries that separate computation from logic and enabling types to express arbitrary mathematical propositions with computational content.

Where previous systems in our exploration maintained strict hierarchies between terms, types, and kinds, CoC dissolves these distinctions. Types become first-class citizens that can be manipulated, passed to functions, and returned as results. This unification enables unprecedented expressiveness while maintaining logical consistency through a carefully constructed universe hierarchy.

## Position in the Lambda Cube

CoC sits at vertex λ2ωP of the lambda cube, combining all three forms of abstraction that define the cube's dimensions:

**Terms depending on Types (\\( \uparrow\\)-axis)**: Polymorphic functions like \\(\mathsf{id} : \forall A : \mathsf{Type}.\; A \to A \\) where terms abstract over type parameters, enabling parametric polymorphism across all types in the system.

**Types depending on Types (\\( \nearrow \\)-axis)**: Type constructors like \\( \mathsf{List} : \mathsf{Type} \to \mathsf{Type} \\) and \\( \Sigma : (A : \mathsf{Type}) \to (A \to \mathsf{Type}) \to \mathsf{Type} \\) where types can abstract over other types, enabling higher-kinded polymorphism and type-level computation.

**Types depending on Terms (\\( \rightarrow \\) -axis)**: Dependent types like \\( \mathsf{Vec} : \mathsf{Nat} \to \mathsf{Type} \to \mathsf{Type} \\) where the structure of types depends on the values of terms, enabling precise specification of data structure properties and program invariants.

The convergence of these three dimensions creates a system of unprecedented expressiveness. Unlike System F-ω, which provides  polymorphism but maintains a clear separation between terms and types, the CoC allows types to depend on arbitrary term-level computations while maintaining decidable type checking through normalization properties.

## The Curry-Howard Correspondence

The CoC realizes the profound connection between computation and logic known as the Curry-Howard correspondence. In this correspondence, types represent logical propositions and terms represent constructive proofs of those propositions. This isomorphism enables the same syntactic framework to express both programs and mathematical theorems with their proofs.

**Propositions as Types**: Every logical statement corresponds to a type. The proposition "for all natural numbers n, n + 0 = n" becomes the type `∀n : Nat. Eq Nat (plus n zero) n`, where `Eq` represents propositional equality.

**Proofs as Programs**: Every constructive proof corresponds to a program that computes a witness for the proposition. A proof of the above proposition becomes a function that takes a natural number and produces evidence that adding zero preserves the number.

**Proof Checking as Type Checking**: Verifying the correctness of a mathematical proof reduces to checking that a program has the expected type. The type checker becomes a proof checker, ensuring that purported proofs actually establish their claimed propositions.

This correspondence transforms programming into theorem proving and theorem proving into programming, creating a unified framework where mathematical rigor and computational efficiency coexist naturally.

## Dependent Types: The Foundation

The key innovation that enables the CoC' expressiveness is the **dependent product type**, or **Π-type**. This construct generalizes the familiar function arrow to create types whose structure depends on the values they abstract over.

### Dependent Products (Π-Types)

The dependent product type \\( \Pi x : A.\; B \\) represents functions where the return type \\( B \\) can depend on the input value \\( x \\). When the variable \\( x \\) does not appear in \\( B \\), this reduces to the simple function type \\( A \to B \\). When \\( x \\) does appear in \\( B \\), we obtain true dependency where the return type varies based on the input value.

```lean
-- Simple function type (non-dependent)
add : Nat → Nat → Nat

-- Dependent function type
vec : (n : Nat) → Type → Type
create_vec : (n : Nat) → (A : Type) → vec n A

-- The return type depends on the input value n
lookup : (n : Nat) → (A : Type) → (v : vec n A) → (i : Fin n) → A
```

The dependent product enables precise type-level specifications that capture program invariants directly in the type system. A vector lookup function can guarantee at compile time that the index falls within the vector bounds, eliminating runtime bounds checking while maintaining type safety.

### Dependent Sums (Σ-Types)

The dependent sum type \\( \Sigma x : A.\; B \\) represents pairs where the type of the second component depends on the value of the first component. This enables existential quantification and the creation of heterogeneous data structures with precise type relationships.

```lean
-- Dependent pair: a number and a vector of that length
sized_vec : Type := Σ n : Nat. vec n Int

-- Create a sized vector
example_vec : sized_vec := ⟨3, [1, 2, 3]⟩

-- Pattern match to extract components
process_vec : sized_vec → Int :=
  fun ⟨n, v⟩ => sum_vector v  -- Type checker knows v has length n
```

Dependent sums enable the expression of existential propositions where we assert the existence of a value with specific properties without revealing the exact value, while maintaining the ability to use that value computationally.

## Universe Hierarchy and Logical Consistency

The power of dependent types raises fundamental questions about self-reference and logical consistency. If types can contain arbitrary values and types themselves are values, what prevents the construction of paradoxical types like the set of all sets that do not contain themselves?

The CoC addresses this challenge through a **universe hierarchy** that stratifies types by their complexity level. Each universe contains types of bounded complexity, and the universe hierarchy prevents the construction of self-referential types that would lead to logical inconsistency.

### Universe Levels

```lean
-- Universe hierarchy
Prop : Type          -- Propositions with no computational content
Type : Type 1        -- Small types (Nat, Bool, etc.)
Type 1 : Type 2      -- Types of type constructors
Type 2 : Type 3      -- Higher-order type constructors
-- ... infinite hierarchy
```

**Prop**: The universe of propositions represents logical statements that, when proven, carry no computational information beyond their truth. Proof irrelevance means that all proofs of the same proposition are considered equal, enabling efficient compilation where proof terms can be erased.

**Type n**: The universe hierarchy `Type 0, Type 1, Type 2, ...` represents computational types at increasing levels of abstraction. Types like `Nat` and `Bool` inhabit `Type 0`, while type constructors like `List : Type 0 → Type 0` inhabit `Type 1`.

**Universe Polymorphism**: Definitions can be polymorphic over universe levels, enabling generic constructions that work across the entire hierarchy. The identity function can be defined once and work for types at any universe level.

The universe hierarchy maintains **predicativity** for computational types, meaning that a type constructor at level n can only quantify over types at levels strictly less than n. This restriction prevents the construction of large elimination paradoxes while maintaining logical consistency.

However, the proposition universe `Prop` is **impredicative**, allowing quantification over arbitrary types including propositions themselves. This impredicativity enables the expression of powerful logical principles while maintaining consistency through proof irrelevance.

## Implementation Architecture

Our Calculus of Constructions implementation demonstrates how these theoretical concepts translate into practical type checking algorithms and programming language features.

```rust
#![enum!("coc/src/ast.rs", Term)]
```

The term language unifies all syntactic categories into a single framework where the same constructs serve multiple roles depending on context. Lambda abstractions create both functions and proof terms, applications represent both function calls and modus ponens, and products represent both function types and universal quantification.

Just as before, our implementation uses  **bidirectional type checking** that splits type checking into complementary synthesis and checking modes.

1. **Synthesis Mode**: Given a term, determines its type by analyzing the term's structure and propagating type information through the syntax tree.
1. **Checking Mode**: Given a term and an expected type, verifies that the term inhabits the expected type by checking compatibility modulo definitional equality.

### Constraint-Based Inference

The system includes advanced constraint solving for implicit argument inference and unification of dependent types. Meta-variables represent unknown types that get resolved through unification with complex dependency tracking.

```rust
#![enum!("coc/src/solver.rs", Constraint)]
```

The constraint solver handles higher-order unification patterns, universe level constraints, and delayed constraint resolution to enable practical programming with dependent types while maintaining theoretical soundness.

## Theoretical Significance

The CoC is more than just another extension of System-F; it has a profound theoretical significance. It essentially demonstrates the fundamental unity between computation and mathematics. Which is one of the most profound ideas of theoretical computer science.

By showing that every constructive mathematical proof corresponds to a program and every program type-checks according to logical principles, CoC bridges the gap between formal verification and practical programming. And thats pretty amazing!

The system also provides a foundation for interactive theorem proving, where mathematical proofs are constructed through programming and verified through type checking. Proof assistants like Coq and Lean build upon the theoretical foundations established by the CoC, demonstrating the practical value of this unification.

Although Coq is interesting historically, we're going to mostly be inspired by the Lean 4 model and adopt a small version of its type system and syntax in our toy implementation.
