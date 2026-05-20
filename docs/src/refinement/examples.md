# Examples

The integration tests are organised as a sequence of `.fun` source files paired with insta snapshots that pin the synthesised type for each top-level expression. Each section below walks through one file and shows what the bidirectional checker and the Z3 backend are doing on the inputs.

## Literals

The first file checks that base literals synthesise their singleton types and that simple arithmetic carries the most precise predicate forward.

```rust
#![source_file!("refinement/tests/01_literals.fun")]
```

The literal `5` synthesises `{ v : Int | v = 5 }`, the literal `true` synthesises `{ v : Bool | v = true }`, and `1 + 2` synthesises `{ v : Int | v = 1 + 2 }`. The expression is not evaluated, which keeps the predicate language closed under composition and lets Z3 do all the arithmetic later. Let bindings introduce the variable into the environment with its synthesised type, so the body of `let x = 5 in x + 1` synthesises `{ v : Int | v = x + 1 }`, with the predicate referring to `x` rather than `5`. The bound name is what carries the refinement forward, not the literal it was bound to.

## Lambdas

Lambdas annotate their argument with a refinement type. The synthesised result type is a dependent arrow that may mention the bound parameter.

```rust
#![source_file!("refinement/tests/02_lambdas.fun")]
```

The identity on positive integers `\x : { n : Int | n > 0 } -> x` synthesises `(x : { n : Int | n > 0 }) -> { v : Int | v > 0 && v = x }`. The result refinement carries both the source-level constraint `v > 0` and the equation `v = x` added by `self_refine` when the variable was looked up. The successor function `\x : Int -> x + 1` synthesises `(x : Int) -> { v : Int | v = x + 1 }`, capturing the arithmetic in the result predicate. The application `(\x : { n : Int | n > 0 } -> x) 3` synthesises `{ v : Int | v > 0 && v = 3 }`, where the literal `3` reflects into the predicate and the precondition `n > 0` is checked by Z3 in the background. The final example introduces the primitive `div`, whose second argument requires non-zero, and shows that the application succeeds against a non-zero literal.

## Path Conditions

The third file is the smallest demonstration that path conditions are tracked.

```rust
#![source_file!("refinement/tests/03_path_conditions.fun")]
```

The expression `\x : Int -> if x > 0 then x else 0 - x` synthesises type `Int -> Int`. The branches both carry path conditions during synthesis, but with no expected type on the lambda the synthesised result is just the common base type. The interesting refinement story for this expression appears in the SMT file below, where the lambda is annotated with the result refinement and the path conditions become the load-bearing facts that make the Z3 check succeed.

## Errors

The error file pins the diagnostics for refinement obligations that fail. Each line is reported with the underlying implication and a Z3 counterexample.

```rust
#![source_file!("refinement/tests/04_errors.fun")]
```

Dividing by the literal zero fails with the obligation `v_1 = 0 ==> v_1 != 0` and the counterexample `v_1 = 0`. Passing an unconstrained `Int` where a non-zero is required fails because Z3 finds the counterexample `x = 0`. Passing the let-bound `y = 0` where a positive is required fails because the path condition carries `y = 0` into the subtyping check. Passing the literal `0 - 5` where a positive is required fails because the synthesised type `{ v : Int | v = 0 - 5 }` reduces in Z3 to a value that fails the predicate `v > 0`. Each counterexample names the offending variables and their values, which is the standard way liquid-types tools report failures.

## SMT Reasoning

The last file exercises Z3's linear arithmetic reasoning across several patterns that show up in any non-trivial refinement program.

```rust
#![source_file!("refinement/tests/05_smt.fun")]
```

The absolute-value annotation `Int -> { v : Int | v >= 0 }` checks by case analysis on the path condition. In the then-branch Z3 has `x >= 0` and the synthesised type is `{ v : Int | v = x }`, so the implication `x >= 0 && v = x ==> v >= 0` is valid. In the else-branch Z3 has `!(x >= 0)` and the synthesised type is `{ v : Int | v = 0 - x }`, so the implication `!(x >= 0) && v = 0 - x ==> v >= 0` is valid. The clamp-to-non-negative function is the same pattern with a literal `0` as the else-branch. The max function shows a dependent result `{ v : Int | v >= y }` checked by case analysis on `x >= y`, with the else-branch satisfying the obligation reflexively.

The chained let-binding `let y = x - 3 in let z = y - 3 in z` against expected result `{ v : Int | v >= 4 }` is the most elaborate use of path conditions. Each let extends the path with an equation between the bound name and the reflected value, so by the time Z3 sees the body it has the conjunction `x >= 10 && y = x - 3 && z = y - 3 && v = z`, from which `v >= 4` follows by linear arithmetic. The transitivity example is similar: from `x > 0` and `y > x` Z3 infers `y > 0` directly. The sum-dominates-first-addend example shows the precondition `x >= 0 && y >= 0 && v = x + y` implying `v >= x`, and the final negative case shows the precondition does not imply `v > x` because `y = 0` is a counterexample.
