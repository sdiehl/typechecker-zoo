# Typechecker Zoo

A cute collection of type checker implementations demonstrating modern type checking algorithms from the last fifty years of programming language design.

[![CI](https://github.com/sdiehl/typechecker-zoo/actions/workflows/ci.yml/badge.svg)](https://github.com/sdiehl/typechecker-zoo/actions/workflows/ci.yml)

<div align="center">

|                                                                                                                       |                                                                                                                     |                                                                                                                |
| :-------------------------------------------------------------------------------------------------------------------: | :-----------------------------------------------------------------------------------------------------------------: | :------------------------------------------------------------------------------------------------------------: |
|   [<img src="./docs/src/images/algorithm-w.png" width="128" height="auto"><br/>**Algorithm W**](./algorithm-w/src)    | [<img src="./docs/src/images/type-classes.png" width="128" height="auto"><br/>**Type Classes**](./type-classes/src) |    [<img src="./docs/src/images/system-f.png" width="128" height="auto"><br/>**System F**](./system-f/src)     |
| [<img src="./docs/src/images/system-f-omega.png" width="128" height="auto"><br/>**System F-ω**](./system-f-omega/src) | [<img src="./docs/src/images/refinement.png" width="128" height="auto"><br/>**Refinement Types**](./refinement/src) | [<img src="./docs/src/images/coc.png" width="128" height="auto"><br/>**Calculus of Constructions**](./coc/src) |
|    [<img src="./docs/src/images/row-poly.png" width="128" height="auto"><br/>**Row Polymorphism**](./row-poly/src)    |  [<img src="./docs/src/images/row-effects.png" width="128" height="auto"><br/>**Row Effects**](./row-effects/src)   |   [<img src="./docs/src/images/cbpv.png" width="128" height="auto"><br/>**Call-by-Push-Value**](./cbpv/src)    |

</div>

### Algorithm W

Robin Milner's classic Hindley-Milner type inference algorithm from _A Theory of Type Polymorphism in Programming_.

### Type Classes

Haskell-style type classes layered on top of Hindley-Milner, with superclasses, default methods, and dictionary-passing elaboration.

Follows the qualified-types presentation from Mark Jones' _Typing Haskell in Haskell_.

### System F

Second-order lambda calculus with parametric polymorphism using bidirectional type checking.

An implementation of bidirectional algorithm from _Complete and Easy Bidirectional Typechecking for Higher-rank Polymorphism_.

### System F-ω

Complete implementation of System F-ω with higher-kinded types, DK bidirectional type checking, existential type variables, polymorphic constructor applications, pattern matching, and lambda expressions with type inference.

Uses the method of _A Mechanical Formalization of Higher-Ranked Polymorphic Type Inference_.

### Refinement Types

A refinement-typed lambda calculus that discharges obligations to Z3 via Rust FFI bindings.

Inspired by the Liquid Types approach of Rondon, Kawaguchi and Jhala.

### Calculus of Constructions

The Calculus of Constructions with a hierarchy of non-cumulative universes, inductive types and universe polymorphism. Limited support for higher-order unification.

Uses a bidirectional dependent typechecker outlined in _A universe polymorphic type system_ by Vladimir Voevodsky.

### Row Polymorphism

Koka-style extensible records with scoped labels: duplicate labels are allowed and shadow on selection.

Inspired by the system from _Extensible records with scoped labels_ by Daan Leijen.

### Row Effects

Algebraic effects and handlers using the same scoped-label row machinery.

### Call-by-Push-Value

A lambda calculus separating values from computations which subsumes both call-by-value and call-by-name. Bidirectional typechecker with two judgment forms.

Basic implementation of Levy's _Call-by-Push-Value: A Functional/Imperative Synthesis_.

## Build

Do the [Rust (latest stable)](https://www.rust-lang.org/tools/install) install then:

```bash
brew install just # or ( cargo install just )
just build        # Build all projects
just test         # Run all tests
```

## Tutorial

The tutorial is built with `mdBook` and `mdbook-include-rs` preprocessor.

```bash
just install-docs  # Install mdbook and dependencies
just build-docs    # Build documentation
just serve-docs    # Serve with live reload
```

## Contributing

If you want to contribute, please fork the repository and submit a pull request.

```shell
# Clone the repository
git clone https://github.com/sdiehl/typechecker-zoo.git
cd typechecker-zoo/docs

# Install mdBook and the include preprocessor
cargo install --git https://github.com/sdiehl/mdbook-include-rs.git

# Start the mdBook preview
just serve-docs

# Make your edits to markdown files in src/
```

Then open a pull request on Github. Any contributions are welcome, especially
typo fixes and improvements. 🙏

## License

MIT Licensed. Copyright 2025-2026 Stephen Diehl.
