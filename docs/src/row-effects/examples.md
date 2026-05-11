# Examples

The integration tests for the row-effects crate are organised as a sequence of `.fun` source files paired with insta snapshots that pin the printed types. Each section below walks through one of those files and explains what the type system is doing on the inputs.

## Pure Values

Pure programs do not invoke any effect, and the printed type drops the effect annotation entirely thanks to the top-level closure step from the [Effect Polymorphism](./effect-polymorphism.md) chapter.

```rust
#![source_file!("row-effects/tests/01_basics.fun")]
```

Literals print without an effect row because the residual effect variable they were inferred with closes to the empty row at the end of inference. Lambdas like `\x -> x` print as `a -> a` rather than `a -<e>-> a` for the same reason: the arrow's effect tail is free at the top level, and the closure step collapses it. Let-bindings whose value is itself pure are eligible for full generalisation, so `let id = \x -> x in id` is `forall a. a -> a` and the underlying scheme covers both the type variable and the effect tail.

## Performing Operations

`perform op e` is how a program invokes an effect. Inference looks up the operation's signature, unifies the argument type, and prepends the operation's label to the effect.

```rust
#![source_file!("row-effects/tests/02_perform.fun")]
```

A single `perform print 1` has type `Unit ! <io>` because `print` is registered with signature `Int -<io>-> Unit`. Inference unifies the argument `1` with `Int`, returns `Unit` for the result, and records `<io>` as the residual effect. Different operations contribute different labels: `read` and `print` both belong to `<io>`, `throw` to `<exn>`, `ask` to `<reader>`, and `tell` to `<writer>`. Sequencing two operations with the same label merges them into one entry of the row, so `let x = perform read () in perform print x` has effect `<io>` rather than `<io, io>`. The last two cases show how the effect attaches to the arrow when the perform is inside a lambda body, with `\x -> perform print x` printing as `Int -<io>-> Unit`.

## Handling Operations

`handle e with op x k -> body` evaluates `e`, discharges the operation's label from `e`'s effect, and binds `x` to the operation's argument and `k` to the continuation that resumes the body.

```rust
#![source_file!("row-effects/tests/03_handle.fun")]
```

The handler `handle perform throw 1 with throw x k -> 0` returns `Int` and has no residual effect because the body's `<exn>` label has been peeled off. Handlers that resume use the continuation argument: `handle perform read () with read x k -> k 7` calls the continuation with `7`, producing the same `Int` result the body would have produced. Pure bodies still type-check under `handle`, because removing a label that is not present in a row is a well-defined operation that simply binds the row's open tail.

## Effect Polymorphism

Pure combinators carry no effect labels but generalise over their effect tails just the same. This is what makes higher-order functions like `apply` usable with both pure and effectful arguments.

```rust
#![source_file!("row-effects/tests/04_polymorphism.fun")]
```

Identity is fully polymorphic in both its type and its effect tail, and prints as `a -> a` because the tail closes at the top level. The K combinator and function composition stay pure in the same way. The let-bound use sites like `let id = \x -> x in let _ = id 1 in id true` show that pure let-bindings generalise correctly and can be reused at different argument types within the same expression, which is the standard let-polymorphism guarantee carried over to the row-polymorphic setting.

## Scoped Labels

Effect rows are scoped in the same sense record rows are scoped: duplicate labels are preserved, order matters, and a handler removes the leftmost occurrence.

```rust
#![source_file!("row-effects/tests/05_scoped_labels.fun")]
```

Sequencing two `print` calls leaves the effect at `<io>` because both operations contribute the same label, which collapses against the row's growing tail. Nested handlers strip labels one layer at a time, so `handle (handle perform throw 1 with throw x k -> 0) with throw x k -> 1` discharges both occurrences of `<exn>` from the inside out. The final example demonstrates effect-row merging across different labels, with `\_ -> let _ = perform read () in perform tell 1` producing the arrow `a -<io, writer>-> Unit` after the row rewriter has exposed both labels through a single shared tail.

## Errors

Failed inferences produce typed error messages rather than panics. The errors file is the negative half of the test suite.

```rust
#![source_file!("row-effects/tests/06_errors.fun")]
```

Unbound variables and unknown operations are rejected before any unification happens. Type mismatches at perform sites fail with the standard `Cannot unify types` message, so `perform print true` reports a unification failure between `Bool` and `Int` because `print`'s signature demands an `Int` argument. Handler returns must agree with the body's result type, so `handle perform read () with read x k -> true` fails because the body returns `Int` and the handler returns `Bool`. The same unification machinery catches mismatches inside lambda bodies, like `(\x -> perform print x) true`.
