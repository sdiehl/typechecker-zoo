# Row Effects

Algebraic effects and handlers extend the type system with a second kind of polymorphism that runs alongside ordinary type polymorphism. Every function arrow carries an effect row recording which operations the function may invoke when called, and inference tracks that row the same way it tracks the result type. A function that performs no effects has an empty row and prints as \\(\tau_1 \to \tau_2\\). A function that performs an effect prints with the row on the arrow, as in `Int -<io>-> Unit`. The same row machinery from the [row polymorphism](../row-poly/overview.md) chapter does all the structural work, with the only difference being that effect labels carry no value payload.

The crate reuses the scoped-label rows from the previous chapter and adapts the operation-and-handler discipline that Pretnar and Plotkin built on top of them. Operations are introduced by `perform op e`, which evaluates `e`, unifies its result with the operation's parameter type, and adds the operation's label to the inferred effect. Handlers are introduced by `handle e with op x k -> body`, which evaluates `e`, removes the operation's label from the effect, and binds `x` to the operation's argument and `k` to the continuation. The typing rules are a row-polymorphic version of Bauer and Pretnar's effect calculus.

The type and effect syntax is

\\[ \begin{align*}
\text{types} \quad \tau &::= \alpha \mid \text{Int} \mid \text{Bool} \mid \text{Unit} \mid \tau_1 \xrightarrow{\varepsilon} \tau_2 \\\\
\text{effects} \quad \varepsilon &::= \cdot \mid e \mid l \mid \varepsilon
\end{align*} \\]

where \\(\cdot\\) is the empty effect row, \\(e\\) is an effect variable, and \\(l \mid \varepsilon\\) prepends a label to a row. The arrow's effect annotation \\(\varepsilon\\) describes what the function may do during a single application, not what evaluating the function value itself does. A lambda value is always pure, but the arrow it produces carries whatever effects the body would invoke when called.

## Example

The smallest program that exercises both effect tracking and the row representation is a lambda that calls a single operation. From the integration snapshot for `02_perform.fun`:

```text
\x -> perform print x
  : Int -<io>-> Unit
```

The parameter `x` enters the environment with a fresh type variable. The body looks up `print`'s signature `Int -<io>-> Unit`, unifies `x`'s type with `Int`, and produces effect `<io | μ>` where `μ` is a fresh tail. Building the lambda places that effect on the arrow, giving the intermediate type `Int -<io | μ>-> Unit`. At the top level the closure step collapses the leftover tail `μ` to the empty row, leaving the printed type `Int -<io>-> Unit`.

The next step up shows why the row representation matters. Sequencing two operations with different labels forces the rows to merge:

```text
\_ -> let _ = perform read () in perform tell 1
  : a -<io, writer>-> Unit
```

The body performs `read` with effect `<io | μ_1>` and `tell` with effect `<writer | μ_2>`. Merging the two rows requires the row rewriter to expose `io` in `<writer | μ_2>`, which it does by binding `μ_2 := <io | β>` and producing `<writer, io | β>`. Unifying with `<io | μ_1>` gives the final row `<io, writer | β>`. The lambda wraps that onto the arrow, and the top-level closure collapses `β` to the empty row, producing `a -<io, writer>-> Unit`. This is the row-poly side condition at work in an effects setting: the same algorithm that rejects unsound record bindings rejects unsound effect bindings, and the same fresh-tail rewrite that lets record rows grow lets effect rows grow.

the [Implementation](./implementation.md) chapter covers the row machinery on the effect side and what is new compared to the records chapter (the arrow annotation, the value restriction, the top-level closure step), and the [Examples](./examples.md) chapter walks through the integration suite end to end.
