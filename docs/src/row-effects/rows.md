# Effect Rows

An effect row records which operations a computation may perform. We reuse the scoped-label row machinery from the [row polymorphism chapter](../row-poly/rows.md), but the labels carry no value payload. The label `io` either appears in a row or it does not, and the order of insertion is preserved. The same Wand-style side condition that protects record inference protects effect inference for the same reason.

```rust
#![enum!("row-effects/src/ast.rs", Effect)]
```

`Empty` is the closed row containing no labels. `Var` is a polymorphic tail standing for some unknown effect row. `Extend` adds a label on top of an existing row. Compared to the record `Row` enum the only structural difference is that `Extend` no longer carries a field type. The row `<io, exn | ε>` is `Extend("io", Extend("exn", Var("ε")))`. Two adjacent occurrences of the same label are distinct, so `<io, io | ε>` is not the same row as `<io | ε>`, exactly as in the record case.

## Unification

Effect-row unification is the payload-free specialisation of the record-row algorithm. The structure of the function mirrors `unify_row` in the row-poly crate: variable cases handle the occurs check, the `(Extend, _)` case hoists the head label of the left side through `rewrite_effect`, and the side condition rejects bindings that would force the same tail variable to be bound twice.

```rust
#![function!("row-effects/src/infer.rs", TypeInference::unify_effect)]
```

The tail walk that detects the divergent case is the same shape as `row_tail`, just on `Effect` instead of `Row`.

```rust
#![function!("row-effects/src/infer.rs", effect_tail)]
```

If the tail is in the substitution we raise `RecursiveEffect` instead of looping.

## Rewriting

`rewrite_effect` is the payload-free version of `rewrite_row`. Given a row and a label `l`, it produces an equivalent row whose head is `l` together with a substitution recording how the row was specialised. There is no field type to recover because effect labels are atoms.

```rust
#![function!("row-effects/src/infer.rs", TypeInference::rewrite_effect)]
```

The four cases match the record case. `Empty` cannot expose `l`, so we report a missing label. `Extend(l, rest)` already has `l` at the head, so the residual is `rest` and no substitution is needed. `Extend(l', rest)` with a different label recurses into `rest` and reattaches `l'` to the rewritten residual, preserving scope. `Var(α)` is the open case: we generate a fresh tail `β`, bind `α := <l | β>`, and return `β` as the residual.

The fresh-tail substitution in the open case is what lets effect rows grow during inference. When a body whose effect is `<io | μ>` is unified with another row that needs a `writer` label, `rewrite_effect` extends `μ` to `<writer | β>` and the rows continue to unify against `β`. Without this rewrite, sequencing two computations with disjoint effect labels would fail.

## Worked Example

Sequencing a `read` and a `tell` produces a row containing both labels. From the integration snapshot for `05_scoped_labels.fun`:

```text
\_ -> let _ = perform read () in perform tell 1
  : a -<io, writer>-> Unit
```

The body of the lambda evaluates `perform read ()` with effect `<io | μ1>` and `perform tell 1` with effect `<writer | μ2>`. Merging the two effects unifies them through `rewrite_effect`. To expose `io` in `<writer | μ2>` the rewriter walks past `writer`, hits the open tail `μ2`, and binds `μ2 := <io | β>`, so the row becomes `<writer, io | β>`. Unifying `μ1` with `<writer | β>` gives the final row `<io, writer | β>`. The lambda wraps that into the arrow `Unit -<io, writer | β>-> Unit`, and the top-level closure step collapses the leftover tail `β` to `Empty`, producing the printed type.
