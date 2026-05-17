# Implementation

## Rows

The row is the spine of every record type. A row is one of three things: the closed empty row, a polymorphic tail variable, or an extension that prepends a labelled field to another row. The same data type is reused by the [row effects](../row-effects/implementation.md) crate with the field payload dropped.

```rust
#![enum!("row-poly/src/ast.rs", Row)]
```

`Empty` is the closed row containing no labels. `Var` is a polymorphic tail standing for some unknown row. `Extend` adds a label with its field type on top of an existing row. The row `{x : Int, y : Bool | r}` is `Extend("x", Int, Extend("y", Bool, Var("r")))`. Two occurrences of the same label are distinct, so `{x : Int, x : Bool}` is a record with two `x` fields, the leftmost shadowing the rightmost under selection and restriction.

A record type wraps a row, and the type language is otherwise the small core of variables, base types, and arrows.

```rust
#![enum!("row-poly/src/ast.rs", Type)]
```

Generalisation tracks type and row variables separately because they live in disjoint substitutions and instantiate against different kinds of structure.

```rust
#![struct!("row-poly/src/ast.rs", Scheme)]
```

### Unification

Row unification follows Figure 2 of _Extensible records with scoped labels_. Equating two rows reduces to equating their head labels in turn. When the left side exposes a label `l`, we hoist `l` to the head of the right side, unify the field types, then unify the residual tails.

```rust
#![function!("row-poly/src/infer.rs", TypeInference::unify_row)]
```

The variable cases are standard. A tail variable `Var(α)` unifies with any row that does not mention `α` in its tail, which is the row occurs check. The interesting case is `(Extend(l, t1, rest1), other)`. We call `rewrite_row` to bring `l` to the front of `other`, recovering the matching field type `t2` and a new tail `rest2`, then recurse on the field types and on the tails.

The side condition guards against the divergent case identified by Wand. If `rest1`'s own tail variable was bound by `rewrite_row`, recursing would force the same variable to be bound again and the algorithm would loop. We detect this by walking `rest1` to its tail and checking whether that variable appears in the substitution that `rewrite_row` just produced.

```rust
#![function!("row-poly/src/infer.rs", row_tail)]
```

If the tail is in the substitution we raise `RecursiveRow` instead of looping. The classic offender is a conditional that demands two different label extensions of the same row variable, like `\r -> if c then {x = 1 | r} else {y = 2 | r}`. The two branches force `r` to extend with `x` on one side and `y` on the other, and the side condition rejects this without diverging.

### Rewriting

`rewrite_row` is the row-rewrite rule. Given a row and a label `l`, it produces an equivalent row whose head is `l`, the field type that sits under that head, and a substitution recording how the row was specialised.

```rust
#![function!("row-poly/src/infer.rs", TypeInference::rewrite_row)]
```

`Empty` cannot expose `l`, so we report a missing label. `Extend(l, t, rest)` already has `l` at the head, so the field type is `t`, the residual is `rest`, and no substitution is needed. `Extend(l', t, rest)` with a different label recurses into `rest` and reattaches `l'` to the rewritten residual, which is what preserves scoped-label semantics: a later occurrence of `l` stays buried under any earlier `l'`. `Var(α)` is the open case. We generate a fresh field type `γ` and a fresh tail `β`, bind `α := {l : γ | β}`, and return `γ`, `β`, and the binding as the substitution.

The fresh-tail substitution in the open case is what lets rows grow during inference. When a record whose row is `{x : Int | r}` is unified with another record that needs a `y` label, `rewrite_row` extends `r` to `{y : β_y | β}` and the rows continue to unify against `β`. Without this rewrite, unifying two records with disjoint label sets through a common row variable would fail.

## Records

Records are the surface form built on top of rows. A record literal `{x = 1, y = true}` has type `{x : Int, y : Bool}`. A row-polymorphic function `\r -> r.x` accepts any record with at least a field `x`, without committing to the rest of the shape. The term language adds four constructs to the lambda calculus core: the empty record, record extension, selection, and restriction.

```rust
#![enum!("row-poly/src/ast.rs", Expr)]
```

`EmptyRecord` is the literal `{}`. `Extend(label, value, rest)` is the desugared form of `{l = e | r}`, prepending a single labelled field to an existing record. A record literal `{x = 1, y = 2}` parses as a chain of `Extend` nodes ending in `EmptyRecord`. `Select(record, label)` is `r.l`, projecting the leftmost field with the matching label. `Restrict(record, label)` is `{r - l}`, removing the leftmost occurrence and exposing whatever lay beneath it.

### Inference

The empty record has type `{}`, the record type wrapping the closed empty row. Extension introduces a fresh row variable for the tail, infers the value and the rest, unifies the rest with `{ρ}` to recover the underlying row, and assembles `{l : τ | ρ}` as the result. Inferring `{x = 1 | r}` for a free `r` gives the principal scheme `forall r. {r} -> {x : Int | r}`, which is what justifies free extension never failing on the right.

Selection has the principal type `forall r a. {l : a | r} -> a`. The inference rule fabricates a fresh field type `α` and a fresh tail `ρ`, then unifies the record's inferred type with `{l : α | ρ}`. If the record already exposes `l` at the head the unifier accepts it directly. If the record's row is a variable the unifier uses `rewrite_row` to bind that variable to `{l : α | ρ}`, which is how `\r -> r.x` becomes row-polymorphic in the rest.

Restriction has the principal type `forall r a. {l : a | r} -> {r}`. The mechanics mirror selection except the result type is the residual record `{ρ}` rather than the field type. Combined with extension this gives an update operation: `{l = new | {r - l}}` overwrites the leftmost `l` in `r` without disturbing fields beneath it.

### Scoped Duplicate Labels

We follow the scoped-label semantics rather than the more common set-of-labels semantics. Duplicate labels are allowed and behave like a stack ordered left to right. Selection always returns the leftmost occurrence, and restriction always peels the leftmost occurrence off, exposing the next one. From the integration snapshot for `02_scoped_labels.fun`:

```text
{x = 1, x = true} : {x : Int, x : Bool}
{x = 1, x = true}.x : Int
{{x = 1, x = true} - x}.x : Bool
```

The first line shows that the duplicate labels are preserved in the type, including their distinct field types. The second line shows that `.x` picks the leftmost, which here is the `Int`. The third line shows that one round of restriction exposes the inner occurrence, which is the `Bool`. The same mechanism lets free extension shadow a polymorphic field type: `\r -> {x = 1 | r}` has type `{r} -> {x : Int | r}`, so applying it to a record that already has `x : Bool` produces `{x : Int, x : Bool}`, not a type error.

The implementation does not need a separate rule for duplicates. `rewrite_row` walks past labels with a non-matching name and stops at the first occurrence of the target label. A later `x` lying behind an earlier `x'` is invisible to the rewriter, which is exactly the behaviour scoped semantics demand. The [Examples](./examples.md) chapter walks through the full record test suite, including extension, restriction, and update.
