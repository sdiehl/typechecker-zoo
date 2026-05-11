# Records

Records are the surface form built on top of rows. A record literal `{x = 1, y = true}` has type `{x : Int, y : Bool}`. A row-polymorphic function `\r -> r.x` accepts any record with at least a field `x`, without committing to the rest of the shape. The term language adds four constructs to the lambda calculus core: the empty record, record extension, selection, and restriction.

```rust
#![enum!("row-poly/src/ast.rs", Expr)]
```

`EmptyRecord` is the literal `{}`. `Extend(label, value, rest)` is the desugared form of `{l = e | r}`, prepending a single labelled field to an existing record. A record literal `{x = 1, y = 2}` parses as a chain of `Extend` nodes ending in `EmptyRecord`. `Select(record, label)` is `r.l`, projecting the leftmost field with the matching label. `Restrict(record, label)` is `{r - l}`, removing the leftmost occurrence and exposing whatever lay beneath it.

## Inference

The empty record has type `{}`, the record type wrapping the closed empty row. Extension introduces a fresh row variable for the tail, infers the value and the rest, unifies the rest with `{ρ}` to recover the underlying row, and assembles `{l : τ | ρ}` as the result. Inferring `{x = 1 | r}` for a free `r` gives the principal scheme `forall r. {r} -> {x : Int | r}`, which is what justifies free extension never failing on the right.

Selection has the principal type `forall r a. {l : a | r} -> a`. The inference rule fabricates a fresh field type `α` and a fresh tail `ρ`, then unifies the record's inferred type with `{l : α | ρ}`. If the record already exposes `l` at the head the unifier accepts it directly. If the record's row is a variable the unifier uses `rewrite_row` to bind that variable to `{l : α | ρ}`, which is how `\r -> r.x` becomes row-polymorphic in the rest.

Restriction has the principal type `forall r a. {l : a | r} -> {r}`. The mechanics mirror selection except the result type is the residual record `{ρ}` rather than the field type. Combined with extension this gives an update operation: `{l = new | {r - l}}` overwrites the leftmost `l` in `r` without disturbing fields beneath it.

## Scoped Duplicate Labels

We follow Leijen's scoped semantics rather than the more common set-of-labels semantics. Duplicate labels are allowed and behave like a stack ordered left to right. Selection always returns the leftmost occurrence, and restriction always peels the leftmost occurrence off, exposing the next one. From the integration snapshot for `02_scoped_labels.fun`:

```text
{x = 1, x = true} : {x : Int, x : Bool}
{x = 1, x = true}.x : Int
{{x = 1, x = true} - x}.x : Bool
```

The first line shows that the duplicate labels are preserved in the type, including their distinct field types. The second line shows that `.x` picks the leftmost, which here is the `Int`. The third line shows that one round of restriction exposes the inner occurrence, which is the `Bool`. The same mechanism lets free extension shadow a polymorphic field type: `\r -> {x = 1 | r}` has type `{r} -> {x : Int | r}`, so applying it to a record that already has `x : Bool` produces `{x : Int, x : Bool}`, not a type error.

The implementation does not need a separate rule for duplicates. `rewrite_row` walks past labels with a non-matching name and stops at the first occurrence of the target label. A later `x` lying behind an earlier `x'` is invisible to the rewriter, which is exactly the behaviour scoped semantics demand.

## Worked Example

A polymorphic extension function reused at two different record shapes is the cleanest demonstration of why row variables exist. From the integration snapshot for `04_extension.fun`:

```text
let push = \r -> {z = 0 | r} in push {x = 1} : {z : Int, x : Int}
let push = \r -> {z = 0 | r} in push {y = true, w = false} : {z : Int, y : Bool, w : Bool}
```

Inferring `push` produces the scheme `forall r. {r} -> {z : Int | r}`. The body extends the parameter with a single `z : Int` field, and generalisation closes over the row variable. At the first use site the argument has type `{x : Int}`, so instantiation picks `r := {x : Int}` and the result is `{z : Int, x : Int}`. At the second use site the argument has type `{y : Bool, w : Bool}`, so instantiation picks `r := {y : Bool, w : Bool}` and the result is `{z : Int, y : Bool, w : Bool}`. The same row-polymorphic function works at two completely different shapes because the row variable absorbs whatever the caller brings.
