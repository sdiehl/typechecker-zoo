# Type Rules

The Calculus of Constructions features an exceptionally rich type system that unifies terms, types, and kinds into a single syntactic category. The typing rules presented here capture the essence of dependent types, universe polymorphism, and the  constraint solving that makes our implementation both powerful and complex.

Unlike simpler type systems, the CoC typing rules must handle multiple layers of abstraction simultaneously. Terms can depend on other terms (functions), types can depend on terms (dependent types), types can depend on types (polymorphism), and even kinds can depend on terms through universe constraints.

## Symbol Glossary

The Calculus of Constructions uses extensive formal notation to capture the complex relationships between terms, types, and universes. This glossary provides a reference for understanding the symbols and concepts used throughout the type rules.

### Core Symbols

- **\\( \Gamma \\)** (Gamma): The context or environment, which tracks what variables are in scope and their types
- **\\( \vdash \\)** (turnstile): The judgment symbol, read as "proves" or "entails"
- **\\( \Pi \\)** (Pi): Dependent function type (generalization of `A -> B` where the result type can depend on the input value)
- **\\( \lambda \\)** (lambda): Function abstraction (creating a function)
- **\\( \equiv \\)**: Definitional equality (two terms that are "the same" according to computation rules)
- **\\( \doteq \\)**: Unification constraint (asking if two terms can be made equal)
- **\\( \leadsto \\)**: Constraint solving (producing a solution to constraints)

### Judgment Forms

- **\\( \Gamma \vdash t : T \\)**: "In context Gamma, term t has type T"
- **\\( \Gamma \vdash T : \text{Type}_i \\)**: "T is a type in universe level i"
- **\\( \Gamma \vdash C \\)**: "Constraint C holds in context Gamma"
- **\\( \Gamma \vdash s \equiv t : T \\)**: "Terms s and t are convertible at type T"

### Type System Concepts

- **Universe hierarchy**: Types live in universes (`Type_0`, `Type_1`, etc.) to avoid logical paradoxes
- **Dependent types**: Types that can depend on values (like "a list of length n")
- **Meta-variables**: Unknowns that our constraint solver tries to figure out (written as `?α`)
- **Substitution**: Replacing variables with values, written as `t[s/x]` (replace x with s in t)
- **Positivity**: A restriction on inductive types to ensure they're well-founded

### Notation Conventions

- **Overlines**: \\( \overline{x} \\) means "a sequence of x's" (like x₁, x₂, x₃, ...)
- **Brackets**: \\( [s/x] \\) means substitution of s for x
- **Subscripts**: \\( \text{Type}_i \\) refers to universe levels
- **Fresh variables**: When we say "fresh(α)", we mean a brand new variable that hasn't been used yet

## Core Judgment Forms

The CoC type system uses several judgment forms that interact in complex ways:

**Typing Judgments**: \\( \Gamma \vdash t : T \\) asserts that term \\( t \\) has type \\( T \\) in context \\( \Gamma \\)

**Universe Judgments**: \\( \Gamma \vdash T : \text{Type}_i \\) asserts that \\( T \\) is a type in universe \\( i \\)

**Constraint Judgments**: \\( \Gamma \vdash C \\) asserts that constraint \\( C \\) holds in context \\( \Gamma \\)

**Conversion Judgments**: \\( \Gamma \vdash s \equiv t : T \\) asserts that \\( s \\) and \\( t \\) are convertible at type \\( T \\)

## Variable and Constant Rules

Variable lookup in the context:

\\[ \frac{x : T \in \Gamma}{\Gamma \vdash x : T} \text{(T-Var)} \\]

Universe hierarchy with explicit level constraints:

\\[ \frac{i < j}{\Gamma \vdash \text{Type}_i : \text{Type}_j} \text{(T-Univ)} \\]

Primitive constants with their types:

\\[ \frac{}{\Gamma \vdash \text{Nat} : \text{Type}_0} \text{(T-Nat)} \\]

\\[ \frac{}{\Gamma \vdash 0 : \text{Nat}} \text{(T-Zero)} \\]

\\[ \frac{}{\Gamma \vdash \text{succ} : \text{Nat} \to \text{Nat}} \text{(T-Succ)} \\]

## Function Types and Abstractions

Dependent function types (Pi types):

\\[ \frac{\Gamma \vdash A : \text{Type}\_i \quad \Gamma, x : A \vdash B : \text{Type}\_j}{\Gamma \vdash \Pi x : A. B : \text{Type}_{\text{max}(i,j)}} \text{(T-Pi)} \\]

Lambda abstraction with dependent types:

\\[ \frac{\Gamma, x : A \vdash t : B \quad \Gamma \vdash \Pi x : A. B : \text{Type}_i}{\Gamma \vdash \lambda x : A. t : \Pi x : A. B} \text{(T-Lam)} \\]

Function application with substitution:

\\[ \frac{\Gamma \vdash f : \Pi x : A. B \quad \Gamma \vdash a : A}{\Gamma \vdash f \; a : B[a/x]} \text{(T-App)} \\]

## Inductive Types

Inductive type formation with universe constraints:

\\[ \frac{\overline{\Gamma \vdash C_i : A_i \to T \; \text{params}} \quad \Gamma \vdash T : \text{Type}_j}{\Gamma \vdash \text{data } T \text{ where } \overline{C_i : A_i} : \text{Type}_j} \text{(T-Data)} \\]

Constructor typing with positivity constraints:

\\[ \frac{\Gamma \vdash I : \text{Type}_i \quad \text{Positive}(I, A)}{\Gamma \vdash c : A \to I} \text{(T-Constr)} \\]

Pattern matching with dependent elimination:

\\[ \frac{\begin{array}{c}
\Gamma \vdash t : I \; \overline{p} \\
\Gamma \vdash P : \Pi \overline{x : A}. I \; \overline{x} \to \text{Type}_k \\
\overline{\Gamma \vdash f_i : \Pi \overline{y : B_i}. P \; (c_i \; \overline{y})}
\end{array}}{\Gamma \vdash \text{match } t \text{ return } P \text{ with } \overline{c_i \; \overline{y} \Rightarrow f_i \; \overline{y}} : P \; t} \text{(T-Match)} \\]

## Universe Polymorphism

Universe variables in types:

\\[ \frac{\alpha \in \text{UVars}}{\Gamma \vdash \text{Type}\_\alpha : \text{Type}_{\alpha+1}} \text{(T-UVar)} \\]

Universe level constraints:

\\[ \frac{\Gamma \vdash C\_1 \quad \Gamma \vdash C\_2}{\Gamma \vdash C\_1 \land C_2} \text{(T-Conj)} \\]


\\[ \frac{i \leq j}{\Gamma \vdash i \leq j} \text{(T-Leq)} \\]

Universe maximum operation:

\\[ \frac{\Gamma \vdash i \leq k \quad \Gamma \vdash j \leq k}{\Gamma \vdash \text{max}(i,j) \leq k} \text{(T-Max)} \\]

## Conversion and Definitional Equality

Beta reduction for function application:

\\[ \frac{}{\Gamma \vdash (\lambda x : A. t) \; s \equiv t[s/x] : B[s/x]} \text{(Conv-Beta)} \\]

Eta conversion for function types:

\\[ \frac{\Gamma \vdash f : \Pi x : A. B \quad x \notin \text{FV}(f)}{\Gamma \vdash f \equiv \lambda x : A. f \; x : \Pi x : A. B} \text{(Conv-Eta)} \\]

Iota reduction for pattern matching:

\\[ \frac{}{\Gamma \vdash \text{match } (c \; \overline{a}) \text{ return } P \text{ with } \overline{c_i \; \overline{y} \Rightarrow f_i \; \overline{y}} \equiv f \; \overline{a} : P \; (c \; \overline{a})} \text{(Conv-Iota)} \\]

Congruence rules for structural conversion:

\\[ \frac{\Gamma \vdash s_1 \equiv t_1 : A \to B \quad \Gamma \vdash s_2 \equiv t_2 : A}{\Gamma \vdash s_1 \; s_2 \equiv t_1 \; t_2 : B} \text{(Conv-App)} \\]

## Type Conversion and Subsumption

Conversion allows definitionally equal types to be used interchangeably:

\\[ \frac{\Gamma \vdash t : A \quad \Gamma \vdash A \equiv B : \text{Type}_i}{\Gamma \vdash t : B} \text{(T-Conv)} \\]

## Meta-Variable and Constraint Rules

Meta-variable introduction for inference:

\\[ \frac{\text{fresh}(\alpha) \quad \Gamma \vdash T : \text{Type}_i}{\Gamma \vdash ?\alpha : T} \text{(T-Meta)} \\]

Constraint solving with unification:

\\[ \frac{\Gamma \vdash s : T \quad \Gamma \vdash t : T \quad \text{Unify}(s, t, \sigma)}{\Gamma \vdash s \doteq t : T \leadsto \sigma} \text{(T-Unify)} \\]

Constraint propagation through substitution:

\\[ \frac{\Gamma \vdash C[\sigma] \quad \text{Dom}(\sigma) \subseteq \text{MetaVars}(C)}{\Gamma \vdash C \leadsto \sigma} \text{(T-Subst)} \\]

These rules capture the  interplay between dependent types, universe constraints, and meta-variable unification that makes the Calculus of Constructions both expressive and challenging to implement. The key insight is that type checking and constraint solving must proceed hand-in-hand, with each phase informing and constraining the other.
