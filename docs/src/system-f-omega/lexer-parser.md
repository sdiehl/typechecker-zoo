# Lexer and Parser

Our System Fω implementation employs a carefully designed parsing strategy that contrasts sharply with the organically evolved complexity found in production Haskell implementations. While Haskell's grammar has grown over decades through endless language extensions and "pragmatic" compromises, resulting in context-dependent parsing, whitespace sensitivity, and intricate layout rules with virtual braces and semicolons, and a general clusterf*** our implementation opts for a much smaller surface langauge.

The Haskell language specification presents significant parsing challenges that have led to numerous implementation variations across different compilers. The language's sensitivity to indentation creates a complex interaction between lexical analysis and parsing phases, where layout rules must insert implicit structure markers. Context-dependent syntax means that identical token sequences can parse differently depending on surrounding declarations, while the proliferation of language extensions has created a patchwork of parsing rules that interact in unexpected ways.

Our toy System Fω implementation deliberately sidesteps these complexities by adopting explicit syntax with clear delimiters, enabling straightforward LALR(1) parsing that produces predictable results independent of context or whitespace variations.

## Lexical Analysis Strategy

The lexical analysis phase transforms source text into a stream of tokens that the parser can process deterministically. Unlike Haskell's context-sensitive lexer that must track indentation levels and insert layout tokens, our lexer operates as a pure finite state machine.

```rust
#![enum!("system-f-omega/src/lexer.rs", Token)]
```

The lexer recognizes several categories of tokens that capture the essential elements of our surface language. Keywords like `data`, `match`, and `forall` establish the syntactic framework for declarations and expressions. Identifiers distinguish between type variables, term variables, and constructor names through naming conventions that the lexer enforces consistently.

Operators receive special treatment to handle the function arrow (`->`) and type signature marker (`::`), both crucial for expressing types and function signatures. Delimiters including parentheses, braces, and semicolons provide explicit structure that eliminates the ambiguity inherent in layout-based syntax.

Numeric and boolean literals complete the token vocabulary, providing the primitive values that serve as the foundation for more complex expressions.

This is not ALL of Haskell, but it's enough to do non-trivial typechecking over. And that's all we really need. The full langauge is, to put it mildly, a bit too large and "organic".
