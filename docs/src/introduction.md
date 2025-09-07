# Typechecker Zoo

This is a pet project of mine I've been working on for a while. We're going to create minimal implementations of the most successful static type systems of the last 50 years. This will involve making toy implementations of programming languages and the core typechecking algorithms. These obviously have evolved a lot over the years, so we'll start with the simple ones and proceed all the way up to modern dependent types. Basically a fun romp through half a century of programming language design.

We're going to implement them all in Rust for no particularly reason, other than Rust having a decent parser ecosystem and being easy to install. And I like the ironic synthesis of building pure functional languages in a language which is decidedly non-functional. It's a bit of a heaven-and-hell thing going on, and I'll leave it up to you to decide on the chirality of that metaphor.

This is going to be a more a fun weekend side project, rather than a formal introduction to these systems. If you want authoritative resources read [TAPL](https://www.cis.upenn.edu/~bcpierce/tapl/), [ATTAPL](https://www.cis.upenn.edu/~bcpierce/attapl/) and [PFPL](http://profs.sci.univr.it/~merro/files/harper.pdf) for the theory and proofs. And also read the primary sources for each typechecker which are linked in the [appendix](./appendices/bibliography.md).

While the textbooks and papers are great, they often focus on the theory in depth and don't cover the gritty details of how to actually implement these kind of typecheckers in real code, in terms of how to lay out the data structures, logic, abstract syntax trees, etc. So we're going for a fun implementation of the gory details of these systems that could be done in a weekend.

The examples are implemented in fairly idiomatic Rust with a full parser and test suite, using the usual compiler libraries such as [lalrpop](https://lalrpop.github.io/lalrpop/), [logos](https://logos.maciej.codes/), [ariadne](https://github.com/zesterer/ariadne), etc. They are obviously simplified and code golfed versions of the full implementations so that they can be easily understood and modified. But they should be a LOT easier to understand than trying to read production implementations. Parsing is also a solved problem, [cranelift](https://cranelift.dev) and [MLIR exists](https://www.stephendiehl.com/posts/mlir_introduction/) so I'm not really going to focus on either because that's increasingly something we offload to libraries.

The four little critters we're going to build are:

<div class="type-system-section">
<a href="./algorithm-w/lambda-calculus.html">
<img src="lambda.png" alt="Lambda calculus symbol" class="type-system-logo">
</a>

[**Algorithm W**](./algorithm-w/lambda-calculus.html) *(775 lines of code)*

Robin Milner's classic Hindley-Milner type inference algorithm from *A Theory of Type Polymorphism in Programming*. A toy **polymorphic lambda calculus**.
</div>

<div class="type-system-section">
<a href="./system-f/system-f.html">
<img src="ocaml.png" alt="OCaml logo" class="type-system-logo">
</a>

[**System F**](./system-f/system-f.html) *(1090 lines of code)*

Second-order lambda calculus with parametric polymorphism using bidirectional type checking. A **Mini-OCaml**

An implementation of DK algorithm from *Complete and Easy Bidirectional Typechecking for Higher-rank Polymorphism* by Dunfield and Krishnaswami.
</div>

<div class="type-system-section">
<a href="./system-f-omega/system-f-omega.html">
<img src="haskell.png" alt="Haskell logo" class="type-system-logo">
</a>

[**System Fω**](./system-f-omega/system-f-omega.html) *(3196 lines of code)*


Complete implementation of System Fω with higher-kinded types, bidirectional type checking, existential type variables, polymorphic constructor applications, pattern matching, and datatypes. A **Haskell-lite**.

Uses the method of *A Mechanical Formalization of Higher-Ranked Polymorphic Type Inference* by Zhao et al.
</div>

<div class="type-system-section">
<a href="./coc/calculus-of-constructions.html">
<img src="lean.png" alt="Lean logo" class="type-system-logo">
</a>

[**Calculus of Constructions**](./coc/calculus-of-constructions.html) *(6000 lines of code)*

The Calculus of Constructions with a countable hierarchy of non-cumulative universes and inductive types. A **teeny Lean-inspired dependent type checker**.

Uses a bidirectional dependent typechecker outlined in *A Universe Polymorphic Type System* by Vladimir Voevodsky.
</div>

This is an MIT licensed project and just something I do as a hobby in my spare time, so if you notice a typo in the prose or code open up a [pull request on Github](https://github.com/sdiehl/typechecker-zoo) and I will be very thankful!
