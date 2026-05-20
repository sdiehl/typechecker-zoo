# Implementation

## Types and Predicates

A type is either a refinement of a base type or a dependent arrow that names its argument so the result can mention it.

```rust
#![enum!("refinement/src/ast.rs", Type)]
```

The `Refine(v, B, phi)` constructor is the type \\(\\{v : B \mid \varphi\\}\\), with three fields: `v` is the value variable bound inside the predicate, `B` is the underlying base type (`Int` or `Bool`), and `phi` is the predicate that must hold of `v`. The `Fun(x, t1, t2)` constructor is the dependent arrow `(x : t1) -> t2`, with three fields: `x` is the binder name that the result type may reference, `t1` is the argument type, and `t2` is the result type. The result `t2` may freely refer to `x` in any predicate, which is what gives the arrow its dependency. The shorthand `Type::base(B)` constructs the unrefined base type as `Refine("v", B, true)`, which is the identity element for the type-level lattice and what the pretty printer collapses to just `B`.

Predicates are the small logical fragment that Z3 understands.

```rust
#![enum!("refinement/src/ast.rs", Pred)]
```

The `Bool(b)` and `Int(n)` constructors wrap boolean and integer literals. The `Var(x)` constructor wraps a logical variable name bound somewhere up the environment chain. The `Bin(op, a, b)` constructor pairs a binary operator with two operand predicates and covers arithmetic, comparison, and boolean operators. The `Not(p)` constructor is logical negation, used both directly in user-written predicates and to encode the false branch of a reflected conditional. The `Neg(p)` constructor is arithmetic negation, distinct from `Not` because it operates at the integer sort rather than the boolean sort. The full operator set lives in `BinOp`, with helpers that classify each operator by its expected argument and result sorts.

```rust
#![enum!("refinement/src/ast.rs", BinOp)]
```

The arithmetic constructors `Add`, `Sub`, and `Mul` take two integer operands and produce an integer. The comparison constructors `Eq`, `Ne`, `Lt`, `Le`, `Gt`, and `Ge` take two integer operands and produce a boolean. The boolean constructors `And`, `Or`, and `Implies` take two boolean operands and produce a boolean. The split between arithmetic, comparison, and boolean operators is what `synth_bin` uses to pick the right argument and result types when checking a binary expression, with the `is_arith`, `is_cmp`, and `is_bool` predicate methods classifying each operator into its sort.

## Synthesis and Checking

Type-checking is bidirectional. The synthesiser takes an expression and produces its most precise type. The checker takes an expression and a target type and produces a unit success or a refinement-failure error.

```rust
#![function!("refinement/src/infer.rs", Checker::synth)]
```

The synthesiser produces singleton types for literals: the literal `5` synthesises `{ v : Int | v = 5 }` rather than the unrefined `Int`, and `true` synthesises `{ v : Bool | v = true }`. Carrying the literal forward in the type is what lets `1 + 2` synthesise `{ v : Int | v = 1 + 2 }`, which is the most precise predicate available without evaluating the expression.

The variable case calls `self_refine` to sharpen the looked-up type with the equation `v = x`, so a let-bound variable `x : { n : Int | n >= 0 }` accessed at a use site has the synthesised type `{ v : Int | v >= 0 && v = x }`. The extra conjunct is what lets the result of a chained computation be expressed in terms of the bound names rather than as a sequence of fresh `v` variables.

Application is where dependent types pay off. The function type is `(x : t1) -> t2`, the argument is checked against `t1`, and the result type substitutes the argument for `x` in `t2`. The substitution is by `subst_ty`, which walks the type and replaces every free `x` in any predicate with the argument expression reflected into the predicate language.

The checker reverses the flow. It takes an expected type and propagates structure inward, switching to synthesis only when no structural rule applies.

```rust
#![function!("refinement/src/infer.rs", Checker::check)]
```

The lambda case unifies argument types by subtyping, extends the environment with the bound parameter at the source-level type, renames the result type's bound variable to the lambda's parameter name, and recurses. The if case checks the condition at `Bool`, then refines the path condition with the reflected condition in the true branch and its negation in the false branch, and checks both branches at the same expected type. The let case synthesises the bound value, extends the environment, and refines the path condition with the equation `x = v` when the bound value is reflectable.

## Reflection

The reflection function is what makes the bridge between expressions and predicates. Many source expressions are pure enough to be lifted into the predicate language verbatim, and that lifting is what lets the type system carry argument expressions through dependent applications.

```rust
#![function!("refinement/src/infer.rs", reflect)]
```

Literals reflect to literal predicates. Variables reflect to predicate variables when their type is a refinement. Arithmetic and boolean operations reflect compositionally. The if expression reflects to a disjunction of two conjunctions, which is the standard encoding of `if c then t else f` in pure logic. Lambdas, applications of non-primitive functions, and let bindings do not reflect, so the result type after applying such an expression simply does not refine its result variable in terms of the argument.

## Path Conditions

Path conditions are the running conjunction of facts that hold along the current control-flow path. When the checker enters the true branch of an `if c then a else b`, the path condition gains a conjunct for the reflected `c`. When it enters the false branch, it gains `!c` instead. Path conditions flow into Z3 as part of the SMT context when subtyping checks fire, so Z3 sees both the type of every variable in scope and the branch conditions along the way.

The path condition is what makes the absolute-value example check.

```text
((\x : Int -> if x >= 0 then x else 0 - x) : Int -> { v : Int | v >= 0 })
```

The lambda is checked against the annotated type. The body is an if whose branches must each have type `{ v : Int | v >= 0 }`. In the true branch the path is `x >= 0`, so subtyping `{ v : Int | v = x } <: { v : Int | v >= 0 }` reduces to the Z3 query "does `x >= 0 && v = x` imply `v >= 0`", which is valid. In the false branch the path is `!(x >= 0)`, the synthesised type is `{ v : Int | v = 0 - x }`, and the obligation `!(x >= 0) && v = 0 - x ==> v >= 0` is valid. Without the path conditions Z3 would have no way to discriminate the two branches and the check would fail.

## Subtyping and the SMT Backend

The whole machinery converges at the subtyping judgment. Subtyping a refinement against another refinement renames both predicates to a common fresh variable, asserts the subtype's predicate, and asks Z3 to prove the supertype's predicate.

```rust
#![function!("refinement/src/infer.rs", Checker::subtype)]
```

The function-type case is the usual contravariant rule: argument types subtype contravariantly, result types covariantly, with the result type's bound variable renamed to align with the supertype's binder. The refinement case is the one that emits SMT obligations. Both predicates are renamed to a fresh `v`, the environment is converted to a list of SMT bindings with their stored refinements as assumptions, the fresh-variable assumption is added, and Z3 is asked whether the goal predicate follows from the conjunction of all the assumptions and the path.

```rust
#![function!("refinement/src/smt.rs", check_valid)]
```

The solver translates each predicate into Z3's `Bool` or `Int` sort, asserts the bindings and the path, asserts the negation of the goal, and asks for `unsat`. An `unsat` answer means the implication is valid. A `sat` answer means there is a counterexample, which is what gets attached to the `RefinementFailed` error. An `unknown` answer is treated as failure, since the refinement system is only sound when its obligations are decidable. This is the only place the implementation depends on Z3, and the translation is small enough to be read in one sitting.
