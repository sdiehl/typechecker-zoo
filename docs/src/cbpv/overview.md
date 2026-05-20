# Call-by-Push-Value

Call-by-push-value is a calculus introduced by Paul Blain Levy that subsumes both call-by-value and call-by-name by splitting every term into one of two syntactic categories. Values are inert and copyable, computations are active and effectful. The slogan is "a value is, a computation does". Functions, returners, and sequencing all live on the computation side. Integers, booleans, pairs, and suspended computations all live on the value side. Two modalities mediate between the two categories. The value type \\(U C\\) is the type of suspended computations of type \\(C\\), so a thunk is a value that wraps a computation. The computation type \\(F A\\) is the type of computations that produce values of type \\(A\\), so a returner is a computation that wraps a value. Every type checker for an effectful language has these two modalities lurking inside it; CBPV makes them syntactically explicit.

The type language has two disjoint judgments. Values inhabit value types, computations inhabit computation types, and the two modalities cross the boundary in opposite directions.

\\[ \begin{align*}
\text{value types} \quad A &::= \text{Int} \mid \text{Bool} \mid \text{Unit} \mid A_1 \times A_2 \mid U\\,C \\\\
\text{comp types} \quad C &::= F\\,A \mid A \to C
\end{align*} \\]

The metavariable \\(A\\) ranges over value types and \\(C\\) ranges over computation types, with the two letters chosen deliberately to remind the reader which judgment a type inhabits. The product former \\(A_1 \times A_2\\) is the type of pairs, with each component a value type because pairs are built from values. The thunk former \\(U\\,C\\) is the value type of suspended computations of type \\(C\\), pronounced "U of C" and read as "the universal type of computations of type \\(C\\)" in Levy's original presentation. The returner former \\(F\\,A\\) is the computation type of producers that yield a value of type \\(A\\), pronounced "F of A" and read as "the free computation type over values of type \\(A\\)". The arrow \\(A \to C\\) lives only on the computation side. Its argument is a value type because functions take values, not computations, and its result is a computation type because applying a function is itself a computation. There is no value-to-value arrow and no computation-to-computation arrow.

To pass a computation as an argument, you suspend it with `thunk` to get a value of type \\(U\\,C\\), and the function takes \\(U\\,C\\) as its argument. To return a value from a computation, you wrap it with `return` to get a computation of type \\(F\\,A\\), and the surrounding code sequences with `to`. The two operators `thunk` and `force` mediate the \\(U\\) modality in opposite directions: `thunk` lifts a computation into a value and `force` lowers a value back to its underlying computation, with the round-trip equation `force (thunk m) = m` holding definitionally. The operators `return` and `to` mediate the \\(F\\) modality the same way: `return` lifts a value into the trivial computation that produces it and `to` sequences a producer into a continuation, with the bind equation `return v to x. n = n[v/x]` holding definitionally.

## Example

The smallest program that exercises both modalities is a thunked returner.

```text
thunk (return 5) : U (F Int)
```

The inner term `return 5` is a computation of type `F Int`: it produces an integer when run. Wrapping it in `thunk` suspends the computation and gives back a value of type `U (F Int)`: a value standing for a delayed integer-producing computation. The thunk can be passed around, stored in a pair, or returned from a function, all of which are value-level operations. To recover the suspended computation, the only operation available is `force`, which converts a `U C` back into the computation `C` it was suspending. The composition `force (thunk m) = m` is a definitional equivalence in the calculus.

The other direction is just as common. A function that adds one to its argument is written as a lambda whose body sequences over its result.

```text
\x : Int. add x 1 : Int -> F Int
```

The body `add x 1` is a primitive computation of type `F Int`. The lambda makes it a computation of type `Int -> F Int`, an arrow from values to computations. There is no version of this lambda that returns an `Int` directly, because the calculus does not allow value-typed computations. Every computation must end in a returner, and the surrounding code must use `to` to extract the value when it wants to do something else with it. This rigidity is the price for making the operational semantics deterministic: at every step you know whether you are pushing a value or running a computation, because the type tells you.

The [Implementation](./implementation.md) chapter walks the bidirectional rules for the two judgments, the role of the \\(U\\) and \\(F\\) modalities, and how primitives fit in. The [Examples](./examples.md) chapter walks the integration test driver line by line, showing what each term synthesises and where the type errors fire.
