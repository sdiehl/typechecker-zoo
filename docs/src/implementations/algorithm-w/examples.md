# Examples

The simplest cases involve literals and variables where type inference is straightforward. Integer literals have type `Int`, boolean literals have type `Bool`, and variables receive the types assigned to them in the typing environment.

Let's examine how our implementation handles these basic cases using the test suite that validates our Algorithm W implementation.

```rust
#![source_file!("algorithm-w/tests/basic.fun")]
```

These examples demonstrate the foundation of type inference. The literal `42` immediately receives type `Int` without any complex reasoning required. Boolean values like `true` and `false` similarly receive type `Bool`. The type system's handling of these base cases forms the building blocks for more complex inference scenarios.

## Function Types and Application

Function types represent the core of lambda calculus and functional programming. When we encounter a lambda abstraction, Algorithm W assigns a fresh type variable to the parameter and infers the type of the body. The resulting function type connects the parameter type with the return type through an arrow.

Function application drives the constraint generation that makes Algorithm W powerful. When applying a function to an argument, the algorithm generates constraints that the function's type must be an arrow from the argument's type to some result type. Unification then solves these constraints.

The identity function `\x -> x` provides the simplest example of polymorphic type inference. Algorithm W assigns a fresh type variable to the parameter `x`, then discovers that the body is just `x` itself. The resulting type `t0 -> t0` captures the essence of the identity function: it accepts any type and returns the same type.

More complex function applications demonstrate how constraints propagate through the system. When we apply the identity function to the integer `42`, the unification process discovers that the type variable `t0` must be `Int`, yielding the final type `Int` for the entire expression.

## Let Polymorphism

Let expressions introduce one of the most  features of the Hindley-Milner type system: let-polymorphism. This mechanism allows variables bound in let expressions to be used with different types in different contexts, enabling flexible code reuse without sacrificing type safety.

The classic example involves binding the identity function in a let expression and then using it with different types. Algorithm W generalizes the type of the bound expression by abstracting over type variables that don't appear free in the current environment. This generalization allows the same binding to be instantiated with fresh type variables at each use site.

Consider the expression `let f = \x -> x in (f 42, f true)`. First, Algorithm W infers that the identity function has type `t0 -> t0`. During generalization, since `t0` doesn't appear in the environment, the system treats this as a polymorphic type that can be instantiated differently at each use.

When the algorithm encounters the first application `f 42`, it creates a fresh instance of the polymorphic type, say `t1 -> t1`, and unifies this with `Int -> t2` (where `t2` is the expected result type). This unification succeeds with `t1 = Int` and `t2 = Int`.

For the second application `f true`, Algorithm W creates another fresh instance `t3 -> t3` and unifies this with `Bool -> t4`. This succeeds with `t3 = Bool` and `t4 = Bool`. The final result is a tuple type `(Int, Bool)`, demonstrating how the same function can be used polymorphically.

## Tuple Types and Complex Data Structures

Tuples provide a way to combine multiple values with potentially different types. Our Algorithm W implementation handles tuples by inferring the type of each component and combining them into a tuple type.

The expression `(42, true)` demonstrates basic tuple construction. Algorithm W infers that the first component has type `Int` and the second has type `Bool`, yielding the tuple type `(Int, Bool)`. This extends naturally to nested tuples like `((1, 2), (true, false))` which receives type `((Int, Int), (Bool, Bool))`.

Tuples interact interestingly with polymorphism. The expression `let f = \x -> (x, x) in f 42` shows how a polymorphic function can construct tuples. The function `f` has type `t0 -> (t0, t0)`, creating a tuple where both components have the same type as the input. When applied to `42`, this yields type `(Int, Int)`.

## Type Error Detection

Algorithm W's constraint-based approach makes it excellent at detecting type errors and providing meaningful error messages. When unification fails, the algorithm can identify exactly where and why types are incompatible.

Attempting to apply a non-function value like `42 true` generates a type error because the integer `42` has type `Int`, but function application requires an arrow type. The unification of `Int` with `Bool -> t0` fails, producing a clear error message.

More subtle errors arise from inconsistent uses of polymorphic functions. While Algorithm W handles let-polymorphism elegantly, it correctly rejects attempts to use functions in incompatible ways within the same scope.

## Complex Inference Scenarios

Real-world programs often involve complex combinations of functions, applications, and data structures that test the full power of Algorithm W. These scenarios demonstrate how the algorithm handles intricate constraint propagation and substitution.

Higher-order functions provide particularly interesting examples. The expression `\f -> \x -> f (f x)` creates a function that applies another function twice. Algorithm W assigns fresh type variables and builds constraints that capture the relationships between all the types involved.

Let's trace through this inference process. The outer lambda receives a fresh parameter type `t0` for `f`. The inner lambda receives type `t1` for `x`. The application `f x` requires that `f` has type `t1 -> t2` for some fresh type `t2`. The outer application `f (f x)` then requires that `f` also has type `t2 -> t3` for the final result type `t3`.

Unification resolves these constraints by discovering that `t2 = t1` and the final type is `(t1 -> t1) -> t1 -> t1`. This captures the essence of function composition: given a function from some type to itself, produce a function that applies it twice.

## The Y-Combinator

The Y-combinator represents a fundamental limitation of the Hindley-Milner type system. This famous fixed-point combinator cannot be typed in our system, illustrating an important boundary between what can be expressed with simple types and what requires more advanced type system features.

The Y-combinator is defined as:

```bash
$ cargo run -- "\\f -> (\\x -> f (x x)) (\\x -> f (x x))"
Type inference error: Occurs check failed: variable 't2' occurs in type t2 → t4
```

The failure occurs when attempting to type the self-application `x x`. When the type checker encounters this expression, it must assign the variable `x` a type that is simultaneously a function type (to be applied) and an argument type (to be passed to itself). This creates the constraint that some type variable `t` must equal `t → τ` for some type `τ`. The occurs check prevents this infinite type, ensuring the type system remains decidable and sound.

This limitation is not a bug but a fundamental characteristic of the Hindley-Milner type system. The Y-combinator requires more advanced type system features such as recursive types or explicit fixed-point operators. Real functional programming languages typically provide built-in recursion constructs like `let rec` that are handled specially by the type checker, avoiding the need for explicit fixed-point combinators at the term level.

The inability to type the Y-combinator illustrates an important trade-off in programming language design between expressiveness and decidability. While more powerful type systems exist that can handle self-application, they come at the cost of increased complexity and potentially undecidable type checking. Algorithm W chooses decidability and simplicity, making it practical for real programming languages while accepting certain expressiveness limitations.

## Limitations: No Mutual Recursion

Our Algorithm W implementation has a significant limitation: it does not support mutual recursion. Mutually recursive definitions occur when two or more functions call each other, creating a circular dependency that requires careful handling during type inference.

Consider this example that would fail in our implementation:

```haskell
let even = \n -> if n == 0 then true else odd (n - 1) in
let odd = \n -> if n == 0 then false else even (n - 1) in
odd 5
```

The problem is that when we encounter the definition of `even`, the function `odd` is not yet in scope, so the reference to `odd` in the body of `even` fails. Standard Algorithm W processes let-bindings sequentially, which makes mutual recursion impossible without additional machinery.

Supporting mutual recursion requires more  techniques:

1. **Dependency Analysis**: The type checker must analyze the dependency graph of definitions to identify strongly connected components (groups of mutually recursive functions).

2. **Simultaneous Inference**: All functions in a mutually recursive group must be typed together, typically by assigning fresh type variables to each function initially, then solving all constraints simultaneously.

3. **Generalization Delays**: The generalization step (which introduces polymorphism) must be delayed until after all mutual dependencies are resolved.

These extensions significantly complicate the implementation and are beyond the scope of our simple Algorithm W demonstration.

## Performance (Or Lack Thereof)

While Algorithm W is theoretically elegant, practical implementations must consider performance characteristics. The algorithm's complexity depends on the size of expressions and the complexity of types involved. Most real programs involve relatively simple type constraints that Algorithm W handles efficiently.

Our implementation demonstrates the core algorithms without the optimizations found in production compilers. Industrial-strength implementations often employ techniques like type-directed compilation, constraint caching, and incremental type checking to handle large codebases efficiently.
