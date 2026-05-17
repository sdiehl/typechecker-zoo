# Implementation

## Rows

An effect row records which operations a computation may perform. We reuse the scoped-label row machinery from the [row polymorphism chapter](../row-poly/implementation.md), but the labels carry no value payload. The label `io` either appears in a row or it does not, and the order of insertion is preserved. The same Wand-style side condition that protects record inference protects effect inference for the same reason.

```rust
#![enum!("row-effects/src/ast.rs", Effect)]
```

`Empty` is the closed row containing no labels. `Var` is a polymorphic tail standing for some unknown effect row. `Extend` adds a label on top of an existing row. Compared to the record `Row` enum the only structural difference is that `Extend` no longer carries a field type. The row `<io, exn | ε>` is `Extend("io", Extend("exn", Var("ε")))`. Two adjacent occurrences of the same label are distinct, so `<io, io | ε>` is not the same row as `<io | ε>`, exactly as in the record case.

### Unification

Effect-row unification is the payload-free specialisation of the record-row algorithm. The structure of the function mirrors `unify_row` in the row-poly crate: variable cases handle the occurs check, the `(Extend, _)` case hoists the head label of the left side through `rewrite_effect`, and the side condition rejects bindings that would force the same tail variable to be bound twice.

```rust
#![function!("row-effects/src/infer.rs", TypeInference::unify_effect)]
```

The tail walk that detects the divergent case is the same shape as `row_tail`, just on `Effect` instead of `Row`.

```rust
#![function!("row-effects/src/infer.rs", effect_tail)]
```

If the tail is in the substitution we raise `RecursiveEffect` instead of looping.

### Rewriting

`rewrite_effect` is the payload-free version of `rewrite_row`. Given a row and a label `l`, it produces an equivalent row whose head is `l` together with a substitution recording how the row was specialised. There is no field type to recover because effect labels are atoms.

```rust
#![function!("row-effects/src/infer.rs", TypeInference::rewrite_effect)]
```

The four cases match the record case. `Empty` cannot expose `l`, so we report a missing label. `Extend(l, rest)` already has `l` at the head, so the residual is `rest` and no substitution is needed. `Extend(l', rest)` with a different label recurses into `rest` and reattaches `l'` to the rewritten residual, preserving scope. `Var(α)` is the open case: we generate a fresh tail `β`, bind `α := <l | β>`, and return `β` as the residual.

The fresh-tail substitution in the open case is what lets effect rows grow during inference. When a body whose effect is `<io | μ>` is unified with another row that needs a `writer` label, `rewrite_effect` extends `μ` to `<writer | β>` and the rows continue to unify against `β`. Without this rewrite, sequencing two computations with disjoint effect labels would fail.

### Worked Example

Sequencing a `read` and a `tell` produces a row containing both labels. From the integration snapshot for `05_scoped_labels.fun`:

```text
\_ -> let _ = perform read () in perform tell 1
  : a -<io, writer>-> Unit
```

The body of the lambda evaluates `perform read ()` with effect `<io | μ1>` and `perform tell 1` with effect `<writer | μ2>`. Merging the two effects unifies them through `rewrite_effect`. To expose `io` in `<writer | μ2>` the rewriter walks past `writer`, hits the open tail `μ2`, and binds `μ2 := <io | β>`, so the row becomes `<writer, io | β>`. Unifying `μ1` with `<writer | β>` gives the final row `<io, writer | β>`. The lambda wraps that into the arrow `Unit -<io, writer | β>-> Unit`, and the top-level closure step collapses the leftover tail `β` to `Empty`, producing the printed type.

## Effect Polymorphism

Every function arrow carries an effect row that describes what the function may do when called. Pure functions carry the empty row and print as `τ1 -> τ2`. Effectful functions print as `τ1 -<ε>-> τ2`.

```rust
#![enum!("row-effects/src/ast.rs", Type)]
```

A scheme generalises both type and effect variables.

```rust
#![struct!("row-effects/src/ast.rs", Scheme)]
```

### Built-in Operations

Each built-in operation has a fixed parameter type, return type, and effect label. `throw` returns a polymorphic type because raising never produces a value of any particular type, so the return slot is filled with a fresh variable at every use site.

```rust
#![function!("row-effects/src/infer.rs", op_signature)]
```

`perform op e` evaluates `e`, unifies its result with the operation's parameter type, and prepends the operation's label to the inferred effect.

### Polymorphic Tails for Pure Terms

A naive base case would give `1 : Int ! <>` (the closed empty row). Sequencing then breaks: unifying `<io>` with `<writer>` cannot succeed because both rows end in `Empty`, and `rewrite_effect` requires an open tail to absorb a new label.

We give pure terms a fresh polymorphic effect tail instead.

```rust
fn fresh_effect(&mut self) -> Effect {
    let v = format!("e{}", self.counter);
    self.counter += 1;
    Effect::Var(v)
}
```

Literals, variables, and lambda values all return `Var(fresh)` as their effect. The lambda body's effect lives on the arrow, not on the lambda value itself, so the value is annotated with a fresh tail. Sequencing two effectful computations now works: `<io | μ1>` unifies with `<writer | μ2>` through `rewrite_effect`, ending in a shared fresh tail.

### Value Restriction

Generalising a let-binding whose evaluation has effects would mean re-running those effects at every use site, which is unsound for an eager language. We use the standard syntactic value restriction.

```rust
#![function!("row-effects/src/infer.rs", is_value)]
```

Only literals, variables, and lambdas are eligible for generalisation. `App`, `Perform`, `Handle`, and nested `Let` are not, even when the inferred effect happens to be a free variable. This matches ML's treatment of mutable references and is the simplest sound rule that admits effect-polymorphic combinators like `apply = \f -> \x -> f x`.

### Closing the Top-Level Row

After inference, free effect variables that nobody constrained are closed to `Empty`. A free tail means "no further labels required", which for display purposes is the same as the closed empty row.

```rust
#![function!("row-effects/src/infer.rs", close_effect_vars)]
```

This collapse hides the polymorphic plumbing introduced for sequencing pure subterms. The expression `1` reports as `Int` instead of `Int ! <e>`. The function `\x -> x` reports as `a -> a` instead of `a -<e>-> a`. Effectful expressions are unaffected because their rows have at least one concrete label and the residual variable is closed beneath that.

### Inference Driver

`infer_type` runs the inference monad, applies the resulting substitution, closes free effect tails, generalises the type, and renames variables for display.

```rust
#![function!("row-effects/src/infer.rs", infer_type)]
```

The renamer assigns `a, b, c, ...` to type variables and `e, f, g, ...` to effect variables. Top-level effect variables that survived closure (none, since we just closed them) and arrow effect variables share the same effect-letter pool, so a polymorphic combinator like `apply` reports its arrow effects as `e` and `f` rather than internal counters.

### Worked Example

A lambda that performs an effect carries the effect on its arrow rather than at the top level. From the integration snapshot for `02_perform.fun`:

```text
\x -> perform print x
  : Int -<io>-> Unit
```

The parameter `x` enters the environment with a fresh type variable. The body `perform print x` looks up `print`'s signature `Int -<io>-> Unit`, unifies `x`'s type with `Int`, and produces effect `<io | μ>`. Building the lambda places that effect on the arrow, giving `Int -<io | μ>-> Unit`, while the lambda value itself takes a fresh effect tail of its own. At the top level, `close_effect_vars` walks the type, sees the arrow's tail `μ` and the lambda's outer tail are both free, and substitutes them to `Empty`. The arrow row becomes `<io>` and the outer row becomes `Empty`, so the printed type drops the `! <...>` suffix and reports the arrow row directly.
