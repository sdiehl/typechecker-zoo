# Dependent Types

Dependent types represent one of the most profound advances in type theory, fundamentally changing the relationship between computation and logic by allowing types to depend on the values they classify. This capability transforms types from static labels into dynamic specifications that can express precise mathematical properties and program invariants directly within the type system.

The journey from simple types to dependent types mirrors the evolution from basic arithmetic to advanced mathematics. Just as mathematics progresses from counting discrete objects to expressing relationships between abstract structures, type systems evolve from classifying basic values to encoding complex logical propositions and computational specifications.

## The Limitation of Simple Types

Traditional type systems, even  ones like System Fω, maintain a fundamental separation between the computational world of terms and the classificatory world of types. Types serve as static labels that group values by their structural properties, enabling compile-time safety checks and optimization opportunities.

```haskell
-- Simple types classify values statically
length : List a -> Int
head : List a -> a  -- Partial function - what if the list is empty?
```

This separation creates a gap between what programmers know about their data and what the type system can express. A programmer might know that a particular list is non-empty, but the type system cannot capture this knowledge, forcing runtime checks that could theoretically be eliminated.

Simple types excel at preventing basic errors like applying functions to arguments of incompatible types, but they cannot express relationships between values or capture domain-specific invariants. The result is a tension between type safety and expressiveness that dependent types resolve by eliminating the artificial boundary between terms and types.

## The Dependent Type Revolution

Dependent types dissolve the separation between computation and classification by allowing types to depend on computational values. This dependency enables types to express arbitrarily precise specifications about the values they classify, transforming the type system into a specification language capable of expressing mathematical theorems.

### Dependent Functions (Π-Types)

The dependent product type `Π x : A. B` generalizes the familiar function arrow `A → B` by allowing the result type `B` to depend on the input value `x`. This dependency enables function types that specify not just the structure of inputs and outputs, but the precise relationship between them.

```lean
-- Non-dependent function type
length : List A → Nat

-- Dependent function type
vec : (n : Nat) → Type → Type
create_vec : (n : Nat) → (A : Type) → vec n A

-- The return type depends on the input value
safe_head : (n : Nat) → (A : Type) → (v : vec (n + 1) A) → A
```

The `safe_head` function demonstrates the power of dependent types: by requiring that the vector length be `n + 1` rather than just any natural number, the type system guarantees that the vector is non-empty, eliminating the possibility of runtime failures when extracting the head element.

### Dependent Pairs (Σ-Types)

The dependent sum type `Σ x : A. B` represents pairs where the type of the second component depends on the value of the first component. This construct enables existential quantification and the creation of data structures that maintain precise relationships between their components.

```lean
-- Simple pair type
Pair A B : Type := A × B

-- Dependent pair type
DPair A B : Type := Σ x : A. B x

-- Concrete example: a vector with its length
sized_vec : Type := Σ n : Nat. vec n Int

-- Pattern matching preserves dependencies
process_sized : sized_vec → Int
process_sized ⟨n, v⟩ = sum_vec v  -- Type checker knows v has length n
```

Dependent pairs enable the expression of existential statements within the type system. Rather than asserting "there exists a natural number n such that property P holds," we can construct a concrete witness that demonstrates the existence while providing computational access to both the witness and the proof of the property.

## Dependent Types as Specifications

The true power of dependent types emerges when we recognize that they function as executable specifications. Unlike traditional specifications written in separate specification languages, dependent types are integrated into the programming language itself, enabling specifications that are checked automatically by the type system.

### Precise Array Bounds

Traditional array operations require runtime bounds checking to ensure memory safety. Dependent types enable compile-time verification of array bounds, eliminating both the runtime overhead and the possibility of bounds violations.

```lean
-- Array type indexed by its length
Array : Nat → Type → Type

-- Bounds-safe array indexing
get : (n : Nat) → (A : Type) → Array n A → (i : Nat) → i < n → A

-- Usage requires proof that index is in bounds
example_access : Array 5 Int → Int
example_access arr = get 5 Int arr 2 (by norm_num)  -- Proof that 2 < 5
```

The type system now captures the relationship between array size and valid indices, transforming a runtime safety property into a compile-time guarantee. Programs that would cause array bounds violations become syntactically ill-formed, preventing an entire class of common programming errors.

### Correctness Conditions

Dependent types can express  correctness conditions that ensure algorithms satisfy their intended properties. A sorting function can be specified to produce a result that is both a permutation of its input and satisfies the sorted property.

```lean
-- Specification of sorted lists
Sorted : List Nat → Prop

-- Specification of permutations
Permutation : List A → List A → Prop

-- Type of correct sorting functions
sort : (l : List Nat) → {l' : List Nat // Sorted l' ∧ Permutation l l'}
```

This specification transforms the informal notion of "correct sorting" into a precise mathematical statement that the type system can verify. Implementations that fail to maintain the required properties will be rejected at compile time, providing strong correctness guarantees.

## The Curry-Howard Correspondence in Practice

Dependent types realize the Curry-Howard correspondence in a practical programming context. The correspondence establishes a deep connection between logical propositions and types, between mathematical proofs and programs, and between proof verification and type checking.

### Propositions as Types

Every mathematical proposition corresponds to a type in the dependent type system. The proposition "for all natural numbers n, n + 0 = n" becomes the type `∀n : Nat, n + 0 = n`, where equality is represented as a type family that is inhabited precisely when its arguments are equal.

```lean
-- Mathematical proposition as a type
zero_right_identity : ∀n : Nat, n + 0 = n

-- Constructive proof as a program
zero_right_identity = fun n =>
  Nat.rec
    (Eq.refl 0)                    -- Base case: 0 + 0 = 0
    (fun k ih => congrArg succ ih) -- Inductive step: (k+1) + 0 = k+1
```

The proof term demonstrates the proposition by providing a computational witness. The type checker verifies that this witness actually establishes the claimed proposition, ensuring that only valid proofs are accepted.

### Programs as Proofs

Conversely, every constructive proof corresponds to a program that computes evidence for the proven proposition. Complex mathematical theorems become  programs that construct witnesses through computation.

```lean
-- Theorem: Every list has a decidable equality test
list_eq_decidable : (A : Type) → [DecidableEq A] → DecidableEq (List A)
list_eq_decidable A inst =
  -- Construction of decidable equality procedure for lists
  -- This is both a proof of decidability and an algorithm for testing equality
```

The program serves dual roles: it provides algorithmic content that can be executed computationally, and it serves as a proof that establishes the mathematical property. This duality eliminates the gap between specification and implementation.

## Challenges and Solutions in Dependent Types

The expressive power of dependent types introduces new challenges that require  solutions. The integration of computation with specification creates complexities that simpler type systems avoid.

### Definitional Equality

In dependent type systems, type checking requires determining when two types are equal. However, since types can contain computational content, type equality becomes computational equivalence, which is generally undecidable.

Practical dependent type systems address this challenge by defining **definitional equality** as equality modulo certain computational rules. Two types are considered definitionally equal if they normalize to identical forms under β-reduction, η-expansion, and other definitional reductions.

```lean
-- These types are definitionally equal
Vector (2 + 3) Int  ≡  Vector 5 Int

-- Because (2 + 3) normalizes to 5
```

This approach maintains decidability by restricting definitional equality to normalizing computations while providing sufficient flexibility for practical programming patterns.

### Universe Hierarchy

The power to construct types that depend on arbitrary values raises logical consistency concerns. If types can contain values and values can be types, what prevents the construction of paradoxical self-referential types?

Dependent type systems maintain consistency through **universe stratification**, where types are organized into a hierarchy of universes with strict inclusion relationships. Each universe contains types of bounded complexity, preventing the construction of impredicative types that would lead to logical paradoxes.

#### Mathematical Formulation

The universe hierarchy can be precisely formulated as an infinite sequence of universes with inclusion relationships:

\\[\mathcal{U}\_0 \subseteq \mathcal{U}\_1 \subseteq \mathcal{U}\_2 \subseteq \cdots \subseteq \mathcal{U}_\omega\\]

Each universe \\(\mathcal{U}_i\\) serves as the domain for types at level \\(i\\), while \\(\text{Type}_i\\) denotes the type of types in universe \\(\mathcal{U}\_i\\). The fundamental typing rules establish the hierarchy:

\\[\frac{A : \text{Type}\_i}{\text{Type}\_i : \text{Type}_{i+1}} \quad \text{(Universe Formation)}\\]

\\[\frac{A : \text{Type}_i \quad i \leq j}{A : \text{Type}_j} \quad \text{(Cumulativity)}\\]

The cumulativity rule enables types to be lifted to higher universes, providing flexibility while maintaining the strict stratification needed for consistency.

#### Type Formation Rules

The universe hierarchy governs how types can be formed at each level. Basic types inhabit universe \\(\mathcal{U}_0\\):

\\[\text{Nat} : \text{Type}_0 \qquad \text{Bool} : \text{Type}_0\\]

Type constructors must respect universe levels. For dependent function types, the result universe is the maximum of the argument and result universes:

\\[\frac{\Gamma \vdash A : \text{Type}\_i \quad \Gamma, x : A \vdash B : \text{Type}\_j}{\Gamma \vdash \Pi x : A. B : \text{Type}_{\max(i,j)}} \quad \text{(Pi Formation)}\\]

For inductive types, the universe level is determined by the constructors' argument types:

\\[\frac{\text{each constructor } c_k \text{ has type } \Pi \overrightarrow{x} : \overrightarrow{A}. T \quad \max(\text{levels}(\overrightarrow{A})) \leq i}{\text{inductive } T : \text{Type}_i} \quad \text{(Inductive Formation)}\\]

#### Predicativity and Consistency

The universe hierarchy ensures **predicativity**, meaning that type constructors at level \\(i\\) can only quantify over types at levels strictly less than \\(i\\). This restriction prevents Russell-style paradoxes:

\\[\frac{\Gamma \vdash A : \text{Type}_i \quad \Gamma, x : A \vdash B : \text{Type}\_j \quad j < i}{\Gamma \vdash \Pi x : A. B : \text{Type}_i} \quad \text{(Predicative Pi)}\\]

Without this restriction, we could construct the type of all types that do not contain themselves, leading to contradiction. The predicativity constraint ensures that self-reference is impossible within the type system.

#### Universe Polymorphism

In our implemenetation we support **universe polymorphism**, allowing definitions to abstract over universe levels:

\\[\text{id} : \Pi u : \text{Level}. \Pi A : \text{Type}_u. A \to A\\]

Universe level variables enable generic programming across the entire universe hierarchy:

\\[\frac{\Gamma \vdash e : \Pi u : \text{Level}. T \quad \ell : \text{Level}}{\Gamma \vdash e[\ell] : T[u := \ell]} \quad \text{(Universe Application)}\\]

#### Universe Arithmetic

Universe polymorphism often requires arithmetic operations on universe levels. We support the following operations:

* **Successor**: \\(u + 1\\) represents the universe immediately above level \\(u\\)
* **Maximum**: \\(\max(u, v)\\) represents the least universe containing both \\(u\\) and \\(v\\)
* **Addition**: \\(u + n\\) represents the universe \\(n\\) levels above \\(u\\)

Which have the following properties:

\\[\max(u, v) = \max(v, u) \quad \text{(Symmetry)}\\]
\\[\max(u, \max(v, w)) = \max(\max(u, v), w) \quad \text{(Associativity)}\\]
\\[(u + m) + n = u + (m + n) \quad \text{(Addition Associativity)}\\]

#### Consistency and Normalization

The universe hierarchy ensures several crucial properties for dependent type systems:

**Strong Normalization**: Every well-typed term has a finite normal form, ensuring that type checking terminates.

**Logical Consistency**: The system admits no proof of false, maintaining its utility as a logical foundation.

**Decidable Type Checking**: The combination of strong normalization and definitional equality makes type checking decidable.

The formal treatment of universes requires careful attention to:

1. **Universe Level Inference**: Automatically determining appropriate universe levels for polymorphic definitions
2. **Constraint Solving**: Resolving systems of universe level constraints that arise during type checking
3. **Cumulative Subtyping**: Implementing the coercion of types to higher universes efficiently

#### Implementation in the Calculus of Constructions

We adopt the following naming convention for type universes:

* **Prop** - The universe of propositions, containing logical statements that may or may not have computational content

* **Type** - The universe of types (equivalent to Type 0), containing ordinary data types like natural numbers and lists

* **Type u** - A universe at level u, where u is a universe level variable

* **Sort u** - A general universe at level u that can represent either Prop (when u = 0) or Type u (when u > 0)

The key principle is that a universe at level \\(u\\) can only classify types that are at levels lower than \\(u\\). This creates a hierarchy that prevents logical paradoxes while enabling expressive type-level programming.

```lean
-- Basic types live in Type (= Type 0)
Nat : Type
Bool : Type
String : Type

-- Type constructors respect universe levels
List : Type → Type
Option : Type → Type

-- Universe hierarchy
Type : Type 1
Type 1 : Type 2
Type 2 : Type 3
-- ... infinite hierarchy

-- Universe polymorphic definitions
def id.{u} (A : Type u) (x : A) : A := x
def compose.{u,v,w} (A : Type u) (B : Type v) (C : Type w)
                    (g : B → C) (f : A → B) (x : A) : C := g (f x)

-- Propositions live in Prop
inductive Eq.{u} {A : Sort u} (a : A) : A → Prop where
  | refl : Eq a a

-- Sort unifies Prop and Type
def identity.{u} {A : Sort u} (x : A) : A := x
```

### Type Inference and Elaboration

The expressiveness of dependent types creates challenges for type inference, as the system must often infer not just types but also proof terms and computational content. Modern dependent type systems employ  **elaboration** processes that insert implicit arguments, resolve type class instances, and construct proof terms automatically.

```lean
-- Surface syntax with implicit arguments
map : {A B : Type} → (A → B) → List A → List B

-- Elaborated form with explicit arguments
map : (A : Type) → (B : Type) → (A → B) → List A → List B

-- Usage with automatic elaboration
result = map succ [1, 2, 3]  -- Elaborates to: map Nat Nat succ [1, 2, 3]
```

The elaboration process bridges the gap between the concise syntax that programmers want to write and the fully explicit form that the type checker requires for verification.
