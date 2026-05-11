# Rows

The row is the spine of every record type. A row is one of three things: the closed empty row, a polymorphic tail variable, or an extension that prepends a labelled field to another row. The same data type is reused by the [row effects](../row-effects/rows.md) crate with the field payload dropped.

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

## Unification

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

## Rewriting

`rewrite_row` is the row-rewrite rule. Given a row and a label `l`, it produces an equivalent row whose head is `l`, the field type that sits under that head, and a substitution recording how the row was specialised.

```rust
#![function!("row-poly/src/infer.rs", TypeInference::rewrite_row)]
```

`Empty` cannot expose `l`, so we report a missing label. `Extend(l, t, rest)` already has `l` at the head, so the field type is `t`, the residual is `rest`, and no substitution is needed. `Extend(l', t, rest)` with a different label recurses into `rest` and reattaches `l'` to the rewritten residual, which is what preserves scoped-label semantics: a later occurrence of `l` stays buried under any earlier `l'`. `Var(α)` is the open case. We generate a fresh field type `γ` and a fresh tail `β`, bind `α := {l : γ | β}`, and return `γ`, `β`, and the binding as the substitution.

The fresh-tail substitution in the open case is what lets rows grow during inference. When a record whose row is `{x : Int | r}` is unified with another record that needs a `y` label, `rewrite_row` extends `r` to `{y : β_y | β}` and the rows continue to unify against `β`. Without this rewrite, unifying two records with disjoint label sets through a common row variable would fail.
