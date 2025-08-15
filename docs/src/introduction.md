# Typechecker Zoo

This is a pet project of mine I've been working on for a while. We're going to create minimal implementations of the most successful static type systems of the last 30 years. This will involve making toy implementations of programming languages and the core typechecking algorithms. These obviously have evolved a lot over the years, so we'll start with the simple ones and proceed all the way up to modern dependent types.

We're going to implement them all in Rust. For no particularly reason, other than it has a decent parser ecosystem and is easy to install. And I like the ironic synthesis of building pure functional languages in a language which is decidedly non-functional. It's a bit of a heaven-and-hell thing going on, and I'll leave it up to you to decide on the chirality of that metaphor.

This is going to be a more a fun weekend side project, than a formal introduction to these systems. If you want resources read [TAPL](https://www.cis.upenn.edu/~bcpierce/tapl/) and [ATTAPL](https://www.cis.upenn.edu/~bcpierce/attapl/) for the theory behind these systems. We'll going for implementation, fun, and rock and roll. But I'll link to the primary resources if you want the gory details.

The four little critters we're going to build are:

<div class="type-system-section">
<a href="./foundations/lambda-calculus.html">
<img src="lambda.png" alt="Lambda calculus symbol" class="type-system-logo">
</a>

[**Algorithm W**](./foundations/lambda-calculus.html) *(775 lines of code)*

Robin Milner's classic Hindley-Milner type inference algorithm from *A Theory of Type Polymorphism in Programming*. A toy **polymorphic lambda calculus**.
</div>

<div class="type-system-section">
<a href="./implementations/system-f/system-f.html">
<img src="ocaml.png" alt="OCaml logo" class="type-system-logo">
</a>

[**System F**](./implementations/system-f/system-f.html) *(1090 lines of code)*

Second-order lambda calculus with parametric polymorphism using bidirectional type checking. A **Mini-OCaml**

An implementation of DK algorithm from *Complete and Easy Bidirectional Typechecking for Higher-rank Polymorphism* by Dunfield and Krishnaswami.
</div>

<div class="type-system-section">
<a href="./implementations/system-f-omega/system-f-omega.html">
<img src="haskell.png" alt="Haskell logo" class="type-system-logo">
</a>

[**System F-ω**](./implementations/system-f-omega/system-f-omega.html) *(3196 lines of code)*

Complete implementation of System F-ω with higher-kinded types, DK bidirectional type checking, existential type variables, polymorphic constructor applications, pattern matching, and datatypes. A **Haskell-lite**.

Uses the method of *A Mechanical Formalization of Higher-Ranked Polymorphic Type Inference* by Zhao et al.
</div>

<div class="type-system-section">
<a href="./implementations/coc/calculus-of-constructions.html">
<img src="lean.png" alt="Lean logo" class="type-system-logo">
</a>

[**Calculus of Constructions**](./implementations/coc/calculus-of-constructions.html) *(6000 lines of code)*

The Calculus of Constructions with a countable hierarchy of non-cumulative universes and inductive types. A **teeny Lean-inspired dependent type checker**.

Uses a bidirectional dependent typechecker outlined in *A Universe Polymorphic Type System* by Vladimir Voevodsky.
</div>
