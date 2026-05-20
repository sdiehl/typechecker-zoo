# Examples

The integration tests live in a single line-oriented driver. Each non-comment line of `cbpv/tests/test_lines.txt` is parsed and type-checked, with the synthesised type or error pinned by an insta snapshot. The sections below walk through groups of lines from that file, showing what the bidirectional checker is doing on each input.

## Values

The first group exercises pure value synthesis without any computations in sight.

```text
5                       : Int
true                    : Bool
()                      : Unit
(1, 2)                  : Int * Int
(true, (1, 2))          : Bool * (Int * Int)
thunk (return 5)        : U (F Int)
```

The literals synthesise their ground value types. The pair `(1, 2)` synthesises `Int * Int` by recursively synthesising each component. The nested pair `(true, (1, 2))` shows that products associate to the right, with the inner pair printed in parentheses to disambiguate from a flat triple. The thunk `thunk (return 5)` is the canonical bridge from computations to values: the inner `return 5` synthesises `F Int`, and `thunk` wraps it in `U`, giving the value type `U (F Int)`. This is the type of a suspended integer-producing computation.

## Computations

The next group exercises the computation judgment. Every term here synthesises a `CompType` and the printed result starts with either `F`, an arrow, or both.

```text
return 5                : F Int
return true             : F Bool
return ()               : F Unit
\x : Int. return x      : Int -> F Int
(\x : Int. return x) 5  : F Int
return 5 to x. return x : F Int
let p = (1, 2) in return p : F (Int * Int)
if true then return 1 else return 2 : F Int
add 2 3                 : F Int
```

The returners wrap their value-typed argument in `F`. The lambda `\x : Int. return x` is the simplest non-trivial computation: it has type `Int -> F Int`, an arrow from a value type to a computation type, which is the only kind of arrow CBPV admits. The application `(\x : Int. return x) 5` reduces to `F Int` because the function's result is `F Int` and the synthesised type after application is exactly the function's result type. The `to` sequencing form `return 5 to x. return x` runs the producer, binds the produced value to `x`, and continues with `return x`, synthesising `F Int` from a chain of two returners. The `let` form `let p = (1, 2) in return p` threads a pair value through to a returner, synthesising `F (Int * Int)`. The conditional `if true then return 1 else return 2` synthesises `F Int` from both branches agreeing on `F Int`. The primitive `add 2 3` synthesises `F Int` because addition has the signature `Int -> Int -> F Int`, with the two integers being values and the addition itself being a computation.

## Thunks and Force

The thunk-and-force pair is the canonical example of the `U`-`F` interplay.

```text
force (thunk (return 5))                : F Int
\f : U (Int -> F Int). force f 5        : U (Int -> F Int) -> F Int
let g = thunk (return 7) in force g     : F Int
(\x : Int. add x 1) 41                  : F Int
return 5 to x. add x x                  : F Int
```

The composition `force (thunk (return 5))` synthesises `F Int` because the thunk wraps a computation of type `F Int`, and forcing it recovers exactly that computation. The lambda `\f : U (Int -> F Int). force f 5` shows the standard idiom for taking a function as an argument in CBPV. The argument has type `U (Int -> F Int)` because functions are computations and to pass a computation as an argument it must be suspended as a thunk. The body forces `f` to recover the underlying arrow, then applies it to `5`, synthesising `F Int`. The complete lambda has type `U (Int -> F Int) -> F Int`. The let-thunk-force pattern `let g = thunk (return 7) in force g` is the value-level analogue. The arithmetic examples show primitives sequencing into compound computations: `(\x : Int. add x 1) 41` applies the successor function and `return 5 to x. add x x` doubles a returner's payload by binding the result and applying `add` to two copies.

## Higher-Order

Two higher-order lambdas test that the arrow type is right-associative and that effect-like signatures compose.

```text
\f : U (Int -> F Int). \x : Int. force f x : U (Int -> F Int) -> Int -> F Int
\b : Bool. if b then return 1 else return 0 : Bool -> F Int
```

The first lambda takes a thunked integer-to-`F Int` function and an integer, forces the function, and applies it. The synthesised type `U (Int -> F Int) -> Int -> F Int` shows that arrows associate to the right in computation types, with each lambda layer contributing one arrow. The second lambda is a CBPV encoding of a boolean-to-integer function: the result must be wrapped in `F` because `if` is a computation form, so the natural Hindley-Milner type `Bool -> Int` becomes `Bool -> F Int` under the CBPV translation.

## Type Errors

The last group pins the diagnostics for ill-typed inputs.

```text
return y                              : ERROR: Unbound variable 'y'
force 5                               : ERROR: Expected a thunk type (U C), found Int
(\x : Int. return x) true             : ERROR: Type mismatch: expected Int, found Bool
if 1 then return 1 else return 2      : ERROR: Type mismatch: expected Bool, found Int
add true 3                            : ERROR: Type mismatch: expected Int, found Bool
```

The unbound variable case fires from `synth_value` when the environment lookup fails. The `force 5` case fires from `synth_comp`'s `Force` arm: synthesising `5` produces `Int`, which is not a thunk type, so `NotAThunk` is raised. The mismatched application checks the argument `true` against `Int` and fails because synthesis produces `Bool` instead. The non-boolean conditional fires from `check_value` on the scrutinee, with `Bool` as the expected type and `Int` as the synthesised one. The mistyped primitive fires the same way, with `add` requiring `Int` arguments and `true` synthesising `Bool`. Each error names the offending position and the mismatched types, which is enough to localise the bug in the source.
