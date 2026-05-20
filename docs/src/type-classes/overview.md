# Type Classes

Type classes are the answer to a question that ordinary Hindley-Milner cannot phrase. The identity function generalises to \\(\forall a.\\, a \to a\\) without any commitment from the caller, but a function that uses equality cannot generalise the same way. Equality on integers, equality on booleans, and equality on pairs are three different procedures, and a single polymorphic `eq` has to carry enough information to pick the right one at the call site. Wadler and Blott's qualified types extend Hindley-Milner with predicates that travel alongside the principal type, so the principal scheme of `eq` becomes \\(\forall a.\\, \text{Eq}\\, a \Rightarrow a \to a \to \text{Bool}\\), and the predicate \\(\text{Eq}\\, a\\) is the part the call site has to discharge.

The crate implements the qualified-type fragment of Mark Jones's _Typing Haskell in Haskell_, restricted to single-parameter classes with optional superclasses and default methods. Types are the familiar HM types extended with a separate syntactic category of predicates, schemes carry a context of predicates as well as a list of quantifiers, and elaboration produces a System F-like core language in which dictionaries are first-class values.

\\[ \begin{align*}
\text{types} \quad \tau &::= \alpha \mid T\\, \overline{\tau} \mid \tau_1 \to \tau_2 \\\\
\text{predicates} \quad \pi &::= C\\, \tau \\\\
\text{qualified} \quad \rho &::= \overline{\pi} \Rightarrow \tau \\\\
\text{schemes} \quad \sigma &::= \forall \overline{\alpha}.\\, \rho
\end{align*} \\]

The metavariable \\(\tau\\) ranges over monotypes built from type variables \\(\alpha\\), type constructors \\(T\\) applied to a sequence of argument types \\(\overline{\tau}\\), and function arrows \\(\tau_1 \to \tau_2\\). The metavariable \\(\pi\\) ranges over predicates, each of the form \\(C\\, \tau\\), which is the claim that the type \\(\tau\\) is an instance of the single-parameter class \\(C\\). The metavariable \\(\rho\\) ranges over qualified types of shape \\(\overline{\pi} \Rightarrow \tau\\), where the overline notation \\(\overline{\pi}\\) denotes a list of predicates (the context) and the fat arrow \\(\Rightarrow\\) separates the context from the underlying monotype. The metavariable \\(\sigma\\) ranges over schemes \\(\forall \overline{\alpha}.\\, \rho\\), which add universal quantifiers \\(\overline{\alpha}\\) over the type variables that the qualified type abstracts. A scheme \\(\forall \overline{\alpha}.\\, \overline{\pi} \Rightarrow \tau\\) reads as "for all \\(\overline{\alpha}\\), assuming the predicates \\(\overline{\pi}\\) hold, the term has type \\(\tau\\)".

The classical Hindley-Milner judgement \\(\Gamma \vdash e : \sigma\\) becomes the qualified-type judgement \\(\overline{\pi} \mid \Gamma \vdash e : \tau\\), where the pending predicates \\(\overline{\pi}\\) accumulate alongside the inferred type. Generalisation lifts pending predicates into the scheme, and instantiation produces fresh predicates whose resolution is deferred to whichever call site is concrete enough to discharge them.

## Example

The two-line program below is the smallest one that exercises every piece of machinery in the crate.

```text
class Eq a where { eq : a -> a -> Bool }
instance Eq Int where { eq = \x -> \y -> primEqInt x y }
\x -> eq x x
```

The class declaration introduces `eq` with the scheme \\(\forall a.\\, \text{Eq}\\, a \Rightarrow a \to a \to \text{Bool}\\). The instance declaration adds a clause to the class environment that says "to prove \\(\text{Eq}\\, \text{Int}\\), use this body for `eq`". The lambda is then inferred at type `a -> Bool` with a pending predicate `Eq a`, which generalisation hoists into the scheme \\(\forall a.\\, \text{Eq}\\, a \Rightarrow a \to \text{Bool}\\). The elaborated core form makes the dictionary passing explicit:

```text
\(d0 : Eq a) -> \x -> d0.eq x x
```

Inferring this requires three operations that ordinary Hindley-Milner does not perform. Inference must collect predicates that arise from instantiating qualified schemes, generalisation must split predicates into those captured by the new scheme and those deferred to the surrounding context, and resolution must search the instance database to discharge a predicate against a ground type. The [Implementation](./implementation.md) chapter explains the predicate-tracking variant of Algorithm W, the [Dictionary Elaboration](./elaboration.md) chapter explains how each abstract construct in the source compiles into an explicit dictionary-passing term in the core, and the [Examples](./examples.md) chapter walks the integration tests one by one.
