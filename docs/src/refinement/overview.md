# Refinement Types

A refinement type is a base type together with a logical predicate over its values. The type \\(\\{v : \text{Int} \mid v > 0\\}\\) is the type of integers that are positive, the type \\(\\{v : \text{Int} \mid v \ne 0\\}\\) is the type of integers that are non-zero, and the type \\(\\{v : \text{Int} \mid v = 5\\}\\) is the singleton type of the literal five. The predicate is part of the type rather than a separate proof obligation, so a type checker can ask its solver "is every \\(v\\) in this set also in that set" when it needs to compare two refinement types. The crate implements the Liquid Types fragment of Rondon, Kawaguchi, and Jhala in the bidirectional style of Cosman and Jhala, with Z3 acting as the decision procedure for the underlying logical theory.

The type language has two constructors. Refinements bind a value variable, fix a base type, and attach a predicate over that variable. Dependent functions bind an argument name and let the result type mention it.

\\[ \begin{align*}
\text{types} \quad \tau &::= \\{v : B \mid \varphi\\} \mid (x : \tau_1) \to \tau_2 \\\\
\text{predicates} \quad \varphi &::= \top \mid n \mid b \mid x \mid \varphi_1 \oplus \varphi_2 \mid \neg \varphi
\end{align*} \\]

The metavariable \\(\tau\\) ranges over types and \\(\varphi\\) ranges over predicates. The refinement former \\(\\{v : B \mid \varphi\\}\\) reads as "the set of values \\(v\\) of base type \\(B\\) such that \\(\varphi\\) holds", with \\(v\\) the bound value variable, \\(B\\) the underlying base type (either \\(\text{Int}\\) or \\(\text{Bool}\\) in this crate), and \\(\varphi\\) a predicate over \\(v\\). The dependent arrow \\((x : \tau_1) \to \tau_2\\) reads as "for every \\(x\\) of type \\(\tau_1\\), a result of type \\(\tau_2\\)", with \\(x\\) the binder name available to \\(\tau_2\\) and \\(\tau_2\\) free to mention \\(x\\) inside any predicate it carries. The truth constant \\(\top\\) is the always-true predicate, used as the predicate of an unrefined base type. The metavariables \\(n\\) and \\(b\\) range over integer and boolean literals, while \\(x\\) ranges over predicate variables drawn from refinement binders or surrounding context. The schematic operator \\(\oplus\\) ranges over the binary operations the decision procedure understands: arithmetic (\\(+, -, \times\\)), comparison (\\(=, \ne, <, \le, >, \ge\\)), and boolean combinators (\\(\wedge, \vee\\)). The unary negation \\(\neg \varphi\\) is logical negation, used both directly and to encode the false branch of a conditional.

Predicates live in the decidable theory of linear integer arithmetic and booleans, which is what makes the subtyping check terminating. The theory is what Z3 reasons in, and the refinement language is exactly large enough to encode any condition that the integer-theory solver can decide.

## Example

The smallest example that exercises every piece of the machinery is integer division.

```text
(\x : { n : Int | n != 0 } -> div 10 x) 2 : Int
```

The argument type pins the binder `x` to the set of non-zero integers. The primitive `div` is declared with the dependent type `(_x : Int) -> { y : Int | y != 0 } -> Int`, so passing `x` to `div 10 ·` succeeds if and only if the type system can prove that every value of type `{ n : Int | n != 0 }` satisfies the predicate `y != 0`. The proof is trivial, but the structure that makes it tractable is the bidirectional discipline: the application is checked, the expected argument type is propagated down to the literal `2`, and the resulting subtyping obligation is discharged to Z3 as the implication `n != 0 ==> y != 0` under a fresh variable. The same machinery rejects `div 10 0` with a counterexample, because the literal `0` synthesises the singleton type `{ v : Int | v = 0 }` and the implication `v = 0 ==> v != 0` is invalid with model `v = 0`.

The [Implementation](./implementation.md) chapter walks the bidirectional rules and shows how path conditions, reflection, and Z3 obligations fit together. The [Examples](./examples.md) chapter walks the integration tests file by file, including the SMT-heavy examples that show Z3 doing arithmetic reasoning across path conditions and chained let-bindings.
