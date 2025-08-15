# Overview

The Calculus of Constructions (CoC) represents a paradigm shift from the systems we have seen so far. It moves beyond the stratified layers of terms, types, and kinds by unifying them into a single, consistent syntactic framework. In CoC, and the dependent type theories that follow it, types are no longer just static labels; they are first-class citizens that can be manipulated, passed to functions, and returned as results, just like any other value. This unification blurs the line between computation and logic, allowing us to express incredibly precise properties of our programs directly within the type system itself.

The central innovation that powers this is the **dependent product type**, or **Pi-type (`Π`)**. It is a generalization of the familiar function arrow (\\( \to \\)). Formally, a Pi-type is written as:

\\[ \Pi x : A . B \\]

This construct has a dual nature. If the variable `x` does *not* appear in the type `B`, then it simplifies to the standard function type \\( A \to B \\). However, if `x` *does* appear in `B`, it becomes a **dependent function type**. This means the type of the function's return value (\\( B \\)) can change based on the *value* of its input (\\( x \\)). For example, we could define a function `create_vector` with the type \\( \Pi n : \text{Nat} . \text{Vector}(\text{Int}, n) \\). This type specifies a function that takes a natural number \\( n \\) and returns a vector of integers whose length is precisely \\( n \\). The type of the output depends on the value of the input.

In our AST, this powerful concept is represented by `Term::Pi(String, Box<Term>, Box<Term>, bool)`. The term that inhabits, or is a "proof" of, a Pi-type is the familiar lambda abstraction, `Term::Abs(String, Box<Term>, Box<Term>)`, written \\( \lambda x : A . t \\). This creates a dependent function whose body \\( t \\) has the type \\( B \\), where \\( B \\) may refer to the value \\( x \\).

This newfound power raises a question: if types like \\( \text{Nat} \\) and \\( \text{Vector} \\) can be passed around like values, what is their type? This leads to the concept of **universes** or **sorts**. A universe is a type that contains other types. The Calculus of Constructions introduces a hierarchy of these universes to maintain logical consistency. This hierarchy includes `Prop` (the type of logical propositions, which have no computational content), `Type_0` (the universe containing data types like \\( \text{Nat} \\)), `Type_1` (the universe containing `Type_0`), and so on, forming an infinite tower. In our AST, `Term::Sort(Universe)` represents a term that is one of these universes. The `Universe` enum itself defines a language for calculating universe levels, which is crucial for preventing paradoxes.

Beyond functions, CoC provides powerful mechanisms for defining data structures from first principles using **inductive types**. An inductive type is defined by its constructors. For instance, \\( \text{Nat} \\) can be defined by its constructors \\( \text{Zero} : \text{Nat} \\) and \\( \text{Succ} : \text{Nat} \to \text{Nat} \\). The `Term::Constructor` variant represents a use of one of these builders. To use values of an inductive type, we need an elimination form, which is what the `Term::Match` construct provides. It allows for pattern matching, deconstructing a value to see which constructor was used to build it.

Finally, CoC includes **dependent pairs**, or **Sigma-types (`Σ`)**, which are the counterpart to Pi-types. A Sigma-type, written \\( \Sigma x : A . B \\), represents a pair where the type of the second element (\\( B \\)) can depend on the value of the first element (\\( x \\)). For example, \\( \Sigma n : \text{Nat} . \text{Vector}(\text{Int}, n) \\) is the type of a pair containing a number \\( n \\) and a vector of that specific length. Our AST captures this with `Term::Sigma`, and its inhabitants are created with `Term::Pair`, and deconstructed with `Term::Fst` and `Term::Snd`.

The `Term` enum, therefore, describes a rich, unified language where programming and proving are two sides of the same coin. Terms like `Abs` and `App` handle computation, while `Pi`, `Sigma`, and `Sort` allow for expressing intricate logical properties. `Constructor` and `Match` give us the power to define our own data worlds.

## Inductive Data Types

## Structures

## Universe Polymorphism
