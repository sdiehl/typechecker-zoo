# Effect Rows

An effect row records which operations a computation may perform. We use the same scoped-label rows that the `row-poly` crate uses for records. The only difference is that effect labels carry no value payload. The label `io` either appears in a row or it does not, and order of insertion is preserved.

```rust
#![enum!("row-effects/src/ast.rs", Effect)]
```

`Empty` is the closed row containing no labels. `Var` is a polymorphic tail standing for some unknown row. `Extend` adds a label on top of an existing row. The row `<io, exn | ε>` is `Extend("io", Extend("exn", Var("ε")))`. Two adjacent occurrences of the same label are distinct, exactly as in Leijen scoped-row records, so `<io, io | ε>` is not the same row as `<io | ε>`.

## Unification

Effect-row unification follows Figure 2 of _Extensible records with scoped labels_. Equating two rows reduces to equating their head labels in turn. When the LHS exposes a label `l`, we hoist `l` to the head of the RHS, then unify the residual tails.

```rust
#![function!("row-effects/src/infer.rs", TypeInference::unify_effect)]
```

The variable cases are standard: a tail variable `Var(α)` unifies with any row that does not mention `α` in its tail (the effect occurs check). The interesting case is `(Extend(l, rest1), other)`. We call `rewrite_effect` to bring `l` to the front of `other`, producing a new tail `rest2`, then recurse on `rest1` and `rest2`.

The side condition guards against the divergent case identified by Wand. If `rest1`'s own tail variable was bound by `rewrite_effect`, recursing would force the same variable to be bound a second time and the algorithm would loop. We detect this by walking `rest1` to its tail and checking whether that variable appears in the substitution `rewrite_effect` produced.

```rust
#![function!("row-effects/src/infer.rs", effect_tail)]
```

If the tail is in the substitution, we raise `RecursiveEffect` instead of looping.

## Rewriting

`rewrite_effect` is the row-rewrite rule. Given a row and a label `l`, it produces an equivalent row whose head is `l`, plus a substitution that records how the row was specialised.

```rust
#![function!("row-effects/src/infer.rs", TypeInference::rewrite_effect)]
```

The four cases:

`Empty` cannot expose `l`, so we report a missing label. `Extend(l, rest)` already has `l` at the head, so the residual is `rest` and no substitution is needed. `Extend(l', rest)` with `l' ≠ l` recurses into `rest` and reattaches `l'` to the rewritten residual, preserving scope. `Var(α)` is the open case: we generate a fresh tail `β`, bind `α := <l | β>`, and return `β` as the residual.

The fresh-tail substitution in the open case is what lets effect rows grow during inference. When a body whose effect is `<io | μ>` is unified with another row that needs a `writer` label, `rewrite_effect` extends `μ` to `<writer | β>` and the rows continue to unify against `β`. Without this rewrite, sequencing two computations with disjoint effect labels would fail.

## Worked Example

Sequencing a `read` and a `tell` produces a row containing both labels. From the integration snapshot for `05_scoped_labels.fun`:

```text
\_ -> let _ = perform read () in perform tell 1
  : a -<io, writer>-> Unit
```

The body of the lambda evaluates `perform read ()` with effect `<io | μ1>` and `perform tell 1` with effect `<writer | μ2>`. Merging the two effects unifies them through `rewrite_effect`. To expose `io` in `<writer | μ2>`, the rewriter walks past `writer`, hits the open tail `μ2`, and binds `μ2 := <io | β>`, so the row becomes `<writer, io | β>`. Unifying `μ1` with `<writer | β>` gives the final row `<io, writer | β>`. The lambda wraps that into the arrow `Unit -<io, writer | β>-> Unit`, and the top-level closure step collapses the leftover tail `β` to `Empty`, producing the printed type.
