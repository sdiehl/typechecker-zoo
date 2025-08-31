# System F Type Checker

While the simple lambda calculus with HM-style type schemes introduces safety and limited polymorphism, it is also very rigid and not very expressive. So now we move onto System F, also known as the polymorphic lambda calculus, smashes this limitation by introducing **parametric polymorphism**: the ability to write a single piece of code that is generic over types. This is the theoretical foundation for generics in languages like Rust, Java, and C++, and it represents a monumental leap in expressive power.

To achieve this, System F extends both the term and type languages. The most crucial addition to the type language is the universal quantifier, \\(\forall\\) (forall), which allows us to express the type of a polymorphic function.

\\[ \begin{align*} \text{types} \quad \tau &::= \alpha \mid \tau_1 \to \tau_2 \mid \forall \alpha . \tau \mid \text{Int} \mid \text{Bool} \\\\ \text{expressions} \quad e &::= x \mid \lambda x : \tau . e \mid e_1 \ e_2 \mid \Lambda \alpha . e \mid e[\tau] \mid \dots \end{align*} \\]

The type \\(\forall \alpha . \tau\\) is represented by `Type::Forall(String, Box<Type>)`. The term language now includes two new constructs specifically for handling polymorphism:

1.  **Type Abstraction (\\(\Lambda \alpha . e\\))**: This creates a polymorphic function. The capital lambda (\\(\Lambda\\)) signifies that we are abstracting over a *type variable* \\(\alpha\\), not a term variable. This expression is represented by `Expr::TAbs(String, Box<Expr>)`.
2.  **Type Application (\\(e[\tau]\\))**: This specializes a polymorphic function by applying it to a concrete type \\(\tau\\). It's how we use a generic function. This is represented by `Expr::TApp(Box<Expr>, Box<Type>)`.

A key difference from the untyped lambda calculus is that our term-level abstractions (\\(\lambda\\)) are now explicitly annotated: \\(\lambda x : \tau . e\\). The programmer must declare the type of the function's parameter. This is a hallmark of System F; its power comes at the cost of full type inference, necessitating these annotations.

The quintessential example of System F's power is the polymorphic identity function, `id`. We can now write a single identity function that works for any type \\(\alpha\\). We create it with a type abstraction and give its parameter `x` the generic type \\(\alpha\\):

\\[ \text{id} = \Lambda \alpha . \lambda x : \alpha . x \\]

The type of this function is \\(\forall \alpha . \alpha \to \alpha\\). This type says: "For all types \\(\alpha\\), I am a function that takes an argument of type \\(\alpha\\) and returns a value of type \\(\alpha\\)."

To use this polymorphic function, we apply it to a type using type application. For example, to get an identity function specifically for integers, we apply `id` to the type \\(\text{Int}\\):

\\[ \text{id}[\text{Int}] \\]

This is a computation that happens at the type-checking level. The result of this type application is a new, specialized expression, \\(\lambda x : \text{Int} . x\\), which has the corresponding specialized type \\(\text{Int} \to \text{Int}\\). We can then apply this specialized function to a value, like \\((\lambda x : \text{Int} . x) \ 5\\), which finally reduces to \\(5\\). This separation of type-level application (\\(e[\tau]\\)) and term-level application (\\(e_1 \ e_2\\)) is fundamental to System F. It provides a powerful and safe way to write generic code, but as we have seen, it forces the programmer to be more explicit with annotations, a tradeoff that directly motivates the bidirectional algorithms used in more modern language implementations.

## Abstract Syntax Trees

Let's look at the full abstract syntax tree for both the type language (`enum Type`) and the term language (`enum Expr`):

### The Type Level

```rust
#![enum!("system-f/src/ast.rs", Type)]
```

The `Type` enum defines the grammar for all possible types in our language. It's the vocabulary we use to describe our expressions.

*   `Type::Var(String)`: This represents a simple type variable, like \\(\alpha\\) or \\(\beta\\). These are placeholders for types that will be specified later, often used in polymorphic functions.

*   `Type::ETVar(String)`: This represents an "existential type variable," often written as \\(^\alpha\\). These are a special kind of variable used internally by the type checker, particularly in bidirectional algorithms. They act as placeholders for an unknown type that the algorithm needs to infer.

*   `Type::Arrow(Box<Type>, Box<Type>)`: This is the function type, \\(\tau_1 \to \tau_2\\). It represents a function that takes an argument of the first type and returns a result of the second type.

*   `Type::Forall(String, Box<Type>)`: This is the universal quantifier, \\(\forall \alpha . \tau\\). It is the cornerstone of System F and represents a polymorphic type. It reads: "for all types \\(\alpha\\), the following type \\(\tau\\) holds." The `String` is the name of the type variable \\(\alpha\\) being bound.

*   `Type::Int` and `Type::Bool`: These are primitive, or base, types. They are concrete types that are built directly into the language, representing 64-bit integers and booleans, respectively.

### The Value Level

The `Expr` enum defines the grammar for all runnable expressions or terms. This is the code that performs computations.

```rust
#![enum!("system-f/src/ast.rs", Expr)]
```

*   `Var(String)`: A term-level variable, like `x` or `f`. It refers to a value that is in scope, such as a function parameter or a `let`-bound variable.

*   `App(Box<Expr>, Box<Expr>)`: This is function application, \\(e_1 \ e_2\\). It represents calling the function \\(e_1\\) with the argument \\(e_2\\).

*   `Abs(String, Box<Type>, Box<Expr>)`: This is a typed lambda abstraction, \\(\lambda x : \tau . e\\). It defines an anonymous function. Unlike in the pure lambda calculus, the parameter (`String`) must have an explicit type annotation (`Box<Type>`). The final `Box<Expr>` is the function's body.

*   `TApp(Box<Expr>, Box<Type>)`: This is type application, \\(e[\tau]\\). This is the mechanism for specializing a polymorphic function. The expression \\(e\\) must have a `Forall` type, and this construct applies it to a specific type \\(\tau\\), effectively "filling in" the generic type parameter.

*   `TAbs(String, Box<Expr>)`: This is type abstraction, \\(\Lambda \alpha . e\\). This is how we create a polymorphic function. It introduces a new type variable (`String`) that can be used within the expression body (`Box<Expr>`).

*   `Ann(Box<Expr>, Box<Type>)`: This represents a type annotation, \\(e : T\\). It's an explicit instruction to the type checker, asserting that the expression \\(e\\) should have the type \\(T\\). This is invaluable in a bidirectional system for guiding the inference process and resolving ambiguity.

*   `LitInt(i64)` and `LitBool(bool)`: These are literal values. They represent concrete, primitive values that are "wired into" the language, corresponding to the base types `Int` and `Bool`. They are the simplest form of expression, representing a constant value.
