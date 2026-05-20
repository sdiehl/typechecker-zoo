# Implementation

## Two Syntactic Categories

The AST mirrors the type system's value-computation split. Every term is either a value or a computation, and the two are represented by distinct enums that recursively refer to each other only at the modality boundaries.

```rust
#![enum!("cbpv/src/ast.rs", Value)]
```

The `Var(name, span)` constructor wraps a variable reference, with the span used for error reporting. The `Int(n, span)` and `Bool(b, span)` constructors wrap integer and boolean literals. The `Unit(span)` constructor is the singleton inhabitant of the unit type. The `Pair(a, b, span)` constructor is the product introduction form, with each component itself a value. The `Thunk(c, span)` constructor is the only place where a value embeds a computation: it wraps a `Comp` and the resulting value has type `U C` where `C` is the type of the wrapped computation. The boxed payload on `Pair` and `Thunk` keeps the enum size bounded despite the mutual recursion between `Value` and `Comp`.

```rust
#![enum!("cbpv/src/ast.rs", Comp)]
```

The `Return(v, span)` constructor packages a value as the trivial computation that produces it, with type `F A` where `A` is the type of `v`. The `Force(v, span)` constructor crosses the boundary in the opposite direction: it requires `v` to have a thunk type `U C` and produces the wrapped computation of type `C`. The `Abs(x, t, body, span)` constructor is the lambda form, with `x` the binder name, `t` the binder's annotated value type, and `body` the computation it abstracts over. The `App(m, v, span)` constructor applies the computation `m` to the value `v`; note that the argument position holds a `Value` rather than a `Comp`, which is how the calculus rules out passing computations directly. The `To(m, x, n, span)` constructor is the bind operator for the `F` modality: `m to x. n` runs `m`, binds its produced value to `x`, and continues with `n`. The `Let(x, v, m, span)` constructor is the value-level analogue, threading a value `v` through to a computation body without any monadic structure. The `If(v, m, n, span)` constructor is the conditional, with the scrutinee a value because conditions are pure. The `Prim(op, a, b, span)` constructor is a saturated primitive application, with both operands required to be values.

The type language is similarly split.

```rust
#![enum!("cbpv/src/ast.rs", ValType)]
```

The `Int`, `Bool`, and `Unit` constructors are the ground value types. The `Pair(a, b)` constructor is the product, with each component itself a value type. The `U(c)` constructor is the thunk modality, wrapping a `CompType` into a value type and pronouncing as "U of C".

```rust
#![enum!("cbpv/src/ast.rs", CompType)]
```

The `F(a)` constructor is the returner modality, wrapping a value type into a computation type and pronouncing as "F of A". The `Arrow(a, c)` constructor is the function arrow, with the argument a value type and the result a computation type. The arrow lives only in `CompType`, which is what makes CBPV functions reject computation-typed arguments. To pass a computation, the caller suspends it with `thunk` and passes the resulting `U C` value. The `F` and `U` constructors are the only places where the two type families touch each other, and they do so in opposite directions: `U` lifts a computation type to a value type, `F` lifts a value type to a computation type.

Primitive operations have fixed input and output types.

```rust
#![enum!("cbpv/src/ast.rs", PrimOp)]
```

```rust
#![impl!("cbpv/src/ast.rs", PrimOp)]
```

The arithmetic constructors `Add`, `Sub`, and `Mul` take two `Int` values and return an `Int`. The comparison constructors `Eq` and `Lt` take two `Int` values and return a `Bool`. The `name` method gives the surface-syntax keyword for pretty-printing and parsing, and the `result` method gives the value type that the primitive's `F`-wrapped result delivers. Every primitive is a computation because it has an effect-like signature: its body is not a value, and its result must be sequenced into whatever needs it.

## Bidirectional Checking

Type-checking is bidirectional with four entry points, one for each combination of value or computation against synthesis or checking.

```rust
#![function!("cbpv/src/infer.rs", Checker::synth_value)]
```

Values are simple enough to always synthesise. Literals produce their ground types. Variables look up in the environment. Pairs synthesise each component independently and assemble the product type. Thunks recursively synthesise their wrapped computation and wrap the result in `U`. There is no thunk-against-`U` checking shortcut, because synthesis is always cheap on the value side.

```rust
#![function!("cbpv/src/infer.rs", Checker::check_value)]
```

The checker has only two structural rules: pair against pair, and thunk against `U`. Both push the expected type into the components. Every other value falls through to synthesis followed by an equality check. This matches the bidirectional idiom: structural rules where they help, synthesis otherwise.

The computation side is more interesting because the modalities have to be handled.

```rust
#![function!("cbpv/src/infer.rs", Checker::synth_comp)]
```

`Return v` synthesises `F (synth_value v)`, packaging the produced value's type into the returner type. `Force v` synthesises `synth_value v` and inspects the result: it must be `U C` for some `C`, and the synthesised type is `C`. The `App` case synthesises the function's type, requires it to be an `Arrow(A, C)`, checks the argument against `A`, and returns `C`. The `Abs` case extends the environment with the binder at its annotated value type and recursively synthesises the body, producing an `Arrow` whose argument is the annotation and whose result is the synthesised computation type. The `To` case synthesises the producer, requires the result to be `F A` for some `A`, extends the environment with the bound variable at `A`, and synthesises the continuation. The `Let` case threads a value type through. The `If` case checks the scrutinee at `Bool`, synthesises both branches, and requires them to agree. The `Prim` case checks both arguments at `Int` and returns the primitive's result type wrapped in `F`.

```rust
#![function!("cbpv/src/infer.rs", Checker::check_comp)]
```

The checker has structural rules for lambda against arrow, return against `F`, if-then-else against any computation type, and the sequencing forms against any computation type. Lambda checking compares the annotated argument type against the expected argument type and pushes the expected result type into the body. Return checking pushes the expected value type into the returned value. If checking pushes the expected type into both branches. Sequencing pushes the expected continuation type into the body of the `To` or `Let`. The fall-through is the usual synthesis-then-equality check.

## Driver

The top-level entry point classifies the term as either a value or a computation and dispatches to the appropriate synthesiser.

```rust
#![function!("cbpv/src/infer.rs", infer_type)]
```

The `TypeResult` enum reflects the value-computation split at the result level. A user-facing program reports its result type as one of two judgments depending on whether the top-level term is a value or a computation, and the pretty printer for `TypeResult` keeps the two cases distinct. This is what makes the CBPV reading visible to the user: a thunk is printed as `U (F Int)` rather than as some hidden encoding, and a returner is printed as `F Int` rather than collapsed to `Int`. The discipline keeps the operational reading of every well-typed term unambiguous.
