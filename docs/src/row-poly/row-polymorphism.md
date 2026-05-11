# Row Polymorphism

Row polymorphism is the type-theoretic answer to a recurring question in everyday code: how do we write a function that accepts any record with at least a field `x`, without committing to the rest of the shape? In a system with rows that function has the principal type \\(\forall r\, \alpha .\\, \\{x : \alpha \mid r\\} \to \alpha\\), and the same function works at `{x = 1}`, at `{x = true, y = 2}`, and at any other record carrying an `x` field. The row variable \\(r\\) absorbs whatever extra fields the caller brings, the type variable \\(\alpha\\) is the field's type, and the function is generic over both at once.

The crate implements the system from Daan Leijen's _Extensible records with scoped labels_, which builds row polymorphism on a single representation called a row. A row is an ordered sequence of labels closed off by either the empty row or a row variable, with the syntax

\\[ \begin{align*}
\text{types} \quad \tau &::= \alpha \mid \text{Int} \mid \text{Bool} \mid \tau_1 \to \tau_2 \mid \\{ \rho \\} \\\\
\text{rows} \quad \rho &::= \cdot \mid r \mid l : \tau \mid \rho
\end{align*} \\]

where \\(\cdot\\) is the closed empty row, \\(r\\) is a row variable, and \\(l : \tau \mid \rho\\) prepends a labelled field to a row. Records and effects are both rows, the only difference being that record labels carry value types and effect labels carry nothing. The same crate is reused by the next chapter on row effects, with the payload dropped from the row constructor.

A defining choice of Leijen's calculus is that rows are scoped rather than sets. Two occurrences of the same label are distinct, so `{x : Int, x : Bool}` is a record with two `x` fields rather than an ill-formed type. Selection and restriction always operate on the leftmost occurrence, which means free extension `{l = e | r}` never has to inspect `r` to decide whether `l` is already present. This is what makes row inference fully syntactic and unification-based, with no special-case logic for label collisions and no need for a subtyping judgement.

## A Worked Motivating Example

The smallest example that exercises the machinery is the polymorphic selector. From the integration snapshot for `03_selection.fun`:

```text
\r -> r.x : {x : a | r} -> a
```

Inferring this type requires two operations the lambda calculus alone cannot perform. The algorithm must unify a record whose row is a variable with a record whose row exposes a concrete label, without committing the variable to be exactly that label. It must also reject unsound bindings rather than diverge when two record types meet that each demand a different label of a shared row variable. Both fall out of the row unification rule and the row rewriting rule covered in the next chapter, and the polymorphic selector is the canonical witness that the rules work.

The same selector applied at two different argument shapes generalises in the obvious way:

```text
let getx = \r -> r.x in getx {x = 1} : Int
let getx = \r -> r.x in getx {x = true} : Bool
```

The inferred scheme is \\(\forall r\, \alpha.\\, \\{x : \alpha \mid r\\} \to \alpha\\), and instantiation picks the field type and the residual row independently at each call site. The first call instantiates \\(\alpha := \text{Int}\\) and \\(r := \cdot\\), and the second instantiates \\(\alpha := \text{Bool}\\) and \\(r := \cdot\\). The chapter on records explains the inference rules behind selection, extension, and restriction, and the [Examples](./examples.md) chapter walks through enough programs to show how the machinery composes in practice.
