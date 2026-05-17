# Examples

The integration tests for the row-poly crate are organised as a sequence of `.fun` source files paired with insta snapshots that pin the printed types. Each section below walks through one of those files and explains what the type system is doing on the inputs. The full source of each file is included inline so the prose and the inputs cannot drift apart.

## Basics

The simplest programs construct records, read fields, and build records inside lambdas. Every constructor and selector lines up directly with one of the inference rules from the [Implementation](./implementation.md) chapter.

```rust
#![source_file!("row-poly/tests/01_basics.fun")]
```

The empty record `{}` infers as `{}`, the record type wrapping the closed empty row. A literal `{x = 1, y = true, z = false}` infers as `{x : Int, y : Bool, z : Bool}` because record extension prepends each field's inferred type onto the row. Nested records nest in the obvious way: `{a = {b = 1}}` has type `{a : {b : Int}}`, and chained selection like `{a = {b = 1}}.a.b` projects through both layers to recover `Int`. The lambdas demonstrate that a record literal in a function body refers to its parameters by their fresh type variables, so `\x -> \y -> {a = x, b = y}` is principal at `a -> b -> {a : a, b : b}`.

## Scoped Labels

Scoped labels are the feature that distinguishes this calculus from a set-of-labels presentation. Duplicate labels are allowed and behave like a stack, with selection and restriction always operating on the leftmost occurrence.

```rust
#![source_file!("row-poly/tests/02_scoped_labels.fun")]
```

The record `{x = 1, x = true}` infers as `{x : Int, x : Bool}` because both occurrences are preserved in the row. Selection `{x = 1, x = true}.x` picks the leftmost `Int`, while `{x = true, x = 1}.x` picks the leftmost `Bool`. Restriction peels one occurrence off at a time, so `{{x = 1, x = true} - x}.x` exposes the inner `Bool`. The same scoping rule applies to free extension: shadowing through `\r -> {x = 1 | r}` produces `{r} -> {x : Int | r}`, which when applied to an argument that already has an `x` yields a record with two `x` fields rather than a type error.

## Selection

Selection is the operation that gives row polymorphism its name. Inside a lambda, selecting a field forces the row to be a variable, which generalises into the principal scheme \\(\forall r\, \alpha .\\, \\{l : \alpha \mid r\\} \to \alpha\\).

```rust
#![source_file!("row-poly/tests/03_selection.fun")]
```

Direct selection from a literal like `{x = 42}.x` is trivial: the row exposes `x` at the head and the unifier accepts immediately. The interesting case is `\r -> r.x`, which has type `{x : a | r} -> a`. Inference introduces a fresh field type and a fresh tail, unifies the parameter's row variable with the open shape, and generalisation closes over both new variables. Let-bound selectors like `let getx = \r -> r.x in ...` get the same scheme and can be applied at different record shapes, including ones with extra fields that the selector never inspects.

## Extension

Free extension `{l = e | r}` prepends a field to a record without checking whether `l` already exists. This is what makes the principal scheme of extension \\(\forall r\, \alpha.\\, \alpha \to \\{r\\} \to \\{l : \alpha \mid r\\}\\) sound under scoped labels.

```rust
#![source_file!("row-poly/tests/04_extension.fun")]
```

Extension over the empty record reconstructs the original literal: `{x = 1 | {}}` is `{x : Int}`. Extension over a polymorphic argument is row-polymorphic in the rest, so `\r -> {z = 0 | r}` has type `{r} -> {z : Int | r}`. The two `push` examples show that the same row-polymorphic extension function works at completely different record shapes through let-generalisation, instantiating the row variable once at each call site. Extension that introduces a duplicate label is intentionally allowed: `\r -> {x = true | {x = 1 | r}}` has type `{r} -> {x : Bool, x : Int | r}`, with the two `x` fields preserved in scope order.

## Restriction

Restriction `{r - l}` peels off the leftmost occurrence of `l`, with principal scheme \\(\forall r\, \alpha.\\, \\{l : \alpha \mid r\\} \to \\{r\\}\\).

```rust
#![source_file!("row-poly/tests/05_restriction.fun")]
```

Restriction from a closed record reduces it: `{{x = 1, y = 2} - x}` has type `{y : Int}`. Restriction from a polymorphic record threads the row variable through: `\r -> {r - x}` has type `{x : a | r} -> {r}`. The combination `\r -> {x = true | {r - x}}` is the basis for an update operation that can change a field's type, with the scheme `{x : a | r} -> {x : Bool | r}`. Restriction on a duplicate label exposes the inner occurrence, so `{{x = 1, x = true} - x}.x` returns the `Bool`.

## Update

Update is syntactic sugar for restrict-then-extend: `{l := e | r}` desugars to `{l = e | {r - l}}`. Because the desugaring goes through restriction it is allowed to change a field's type.

```rust
#![source_file!("row-poly/tests/06_update.fun")]
```

Updating preserving the type, like `{x := 99 | p}` against `{x = 1, y = 2}`, gives back `{x : Int, y : Int}`. Heterogeneous update, like `{x := true | p}` against the same `p`, gives `{x : Bool, y : Int}` because the new field type replaces the old. A polymorphic updater `\v -> \r -> {x := v | r}` generalises to a scheme that lets the caller pick both the new value's type and the rest of the row.

## Polymorphism

Let-generalisation closes over both type and row variables. A function that uses a record polymorphically through let can be instantiated independently at each call site.

```rust
#![source_file!("row-poly/tests/07_polymorphism.fun")]
```

The first example, `let getx = \r -> r.x in {a = getx {x = 1}, b = getx {x = true}}`, exercises both row and field polymorphism: the same selector is applied at `{x = 1}` and `{x = true}`, and the result is `{a : Int, b : Bool}`. Records carrying polymorphic values like `{id = \x -> x, fst = \x -> \y -> x}` infer principal field types `{id : a -> a, fst : b -> c -> b}`, with each field's scheme independent. The last entry is the canonical row-polymorphic selector with two independent rows, `\r -> \s -> {a = r.x, b = s.y}`, which has type `{x : a | r} -> {y : b | s} -> {a : a, b : b}`.

## Termination

The side condition in row unification is what rules out the divergent cases identified by Wand. These examples are deliberately the kind of programs that without the side condition would force the algorithm to bind the same tail variable twice.

```rust
#![source_file!("row-poly/tests/09_termination.fun")]
```

The first example builds two extension functions that share a row variable `r` but extend with different labels, then selects only one. Inference must not commit `r` to be both `{x : Int | _}` and `{y : Int | _}` at the same time. Because only `f` is returned, the side condition lets the program type-check with `f`'s scheme `{r} -> a -> {x : Int | r}`, deferring the conflict that never materialises. The genuine conflict shows up in the [errors](./examples.md#errors) file, where forcing the unification produces `RecursiveRow`.

## Errors

Failed inferences produce typed error messages rather than panics. The errors file is the negative half of the test suite: each line is expected to fail.

```rust
#![source_file!("row-poly/tests/10_errors.fun")]
```

Selection of a missing label, like `{x = 1}.y`, reports `Label 'y' missing from row {}`. Restriction of a missing label reports the same. The Wand-style trigger, `\r -> \k -> let dummy = k {x = 1 | r} in k {y = 2 | r}`, forces `{x : Int | r} ~ {y : Int | r}` and is caught by the side condition as `Recursive row unification: tail variable 'r5' would be bound twice`. Selection on a non-record like `(\x -> x.foo) 1` fails with a structural mismatch between `{foo : t2 | r3}` and `Int`, and applying a record where a function is expected fails the same way in the opposite direction.
