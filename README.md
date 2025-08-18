# Typechecker Zoo

A cute collection of type checker implementations demonstrating modern type checking algorithms from the last fifty years of programming language design.

[![CI](https://github.com/sdiehl/typechecker-zoo/actions/workflows/ci.yml/badge.svg)](https://github.com/sdiehl/typechecker-zoo/actions/workflows/ci.yml)

<div align="center">

| | |
|:---:|:---:|
| [<img src="./docs/src/lambda.png" width="128" height="auto"><br/>**Algorithm W**](./algorithm-w/src) | [<img src="./docs/src/ocaml.png" width="128" height="auto"><br/>**System F**](./system-f/src) |
| [<img src="./docs/src/haskell.png" width="128" height="auto"><br/>**System F-ω**](./system-f-omega/src) | [<img src="./docs/src/lean.png" width="128" height="auto"><br/>**Calculus of Constructions**](./coc/src) |

</div>

### Algorithm W

Robin Milner's classic Hindley-Milner type inference algorithm from *A Theory of Type Polymorphism in Programming*.

### System F

Second-order lambda calculus with parametric polymorphism using bidirectional type checking.

An implementation of bidirectional algorithm from *Complete and Easy Bidirectional Typechecking for Higher-rank Polymorphism*.

### System F-ω

Complete implementation of System F-ω with higher-kinded types, DK bidirectional type checking, existential type variables, polymorphic constructor applications, pattern matching, and lambda expressions with type inference.

Uses the method of *A Mechanical Formalization of Higher-Ranked Polymorphic Type Inference*.

### Calculus of Constructions

The Calculus of Constructions with a hierarchy of non-cumulative universes, inductive types and universe polymorphism. Limited support for higher-order unification.

Uses a bidirectional dependent typechecker outlined in *A universe polymorphic type system* by Vladimir Voevodsky.

## Build Instructions

### Prerequisites

* [Rust (latest stable)](https://www.rust-lang.org/tools/install)
* [Just build system](https://just.systems/man/en):
  - `cargo install just`
  - `brew install just`

### Commands

```bash
just build  # Build all projects
just test   # Run all tests
```

## Documentation

Documentation is built with `mdBook` and `mdbook-include-rs` preprocessor.

```bash
just install-docs  # Install mdbook and dependencies
just build-docs    # Build documentation
just serve-docs    # Serve with live reload
```

## License

MIT Licensed. Copyright 2025 Stephen Diehl.
