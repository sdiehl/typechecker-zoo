# Examples

The integration tests are organised as a sequence of `.fun` source files paired with insta snapshots that pin both the qualified type and the elaborated core term. Each section below walks through one of those files and explains what the type system and the elaborator are doing on the inputs.

## Basics

The first file checks that ordinary Hindley-Milner still works in the absence of any class declarations.

```rust
#![source_file!("type-classes/tests/01_basics.fun")]
```

The identity function generalises to `forall a. a -> a` with an empty context, the constant function generalises to `forall a b. a -> b -> a`, and the literals infer at their ground types. The elaborated core is identical to the source because no dictionary abstraction or projection is required. The presence of the class machinery does not change the underlying inference algorithm for class-free programs.

## Classes

A class declaration introduces method names with qualified schemes but no instances yet.

```rust
#![source_file!("type-classes/tests/02_classes.fun")]
```

The expression `eq` infers at the principal scheme \\(\forall a.\\, \text{Eq}\\, a \Rightarrow a \to a \to \text{Bool}\\), elaborating to `\(d0 : Eq a) -> d0.eq`. The partial application `\x -> eq x` carries the same qualified type and elaborates to a lambda wrapped in the same dictionary abstraction. Both forms make the dictionary-passing structure visible: every use of a class method becomes a projection out of a dictionary variable bound by an outer `DictAbs`.

## Instances

Adding an instance lets the resolver discharge the class predicate at concrete types.

```rust
#![source_file!("type-classes/tests/03_instances.fun")]
```

The lambda `\x -> eq x x` infers at `Eq a => a -> Bool` because the variable is left polymorphic. The elaborated form `\(d0 : Eq a) -> \x -> d0.eq x x` carries the dictionary binder forward. The let-bound `f` follows the same shape: its scheme is qualified, so its elaborated body has a `DictAbs`, and the outer expression that returns `f` takes its own dictionary as an outer binder and threads it via `f @d2`. The chain illustrates the central elaboration invariant: every qualified scheme appears in the core as a function over dictionaries.

## Resolution

When an overloaded operation is applied at a ground type, resolution kicks in and the dictionary is built at the call site.

```rust
#![source_file!("type-classes/tests/04_resolution.fun")]
```

The two ground-typed applications elaborate to inline `DictRec` literals: `<Eq Int: eq = ...>.eq 1 2` and `<Eq Bool: eq = ...>.eq true false`. Each call site picks its instance independently, and the dictionary is constructed where it is needed rather than passed as an argument. The final example shows resolution composing with let-generalisation: `test` is generalised with the predicate `Eq a`, and applying it to `42` instantiates `a := Int` and discharges the predicate with the `Eq Int` dictionary, threading it in as a `@(<Eq Int: ...>)` argument.

## Superclasses

A class can declare a superclass, which means that any instance of the subclass must also have an instance of the superclass, and a dictionary for the subclass can produce a dictionary for the superclass on demand.

```rust
#![source_file!("type-classes/tests/05_superclass.fun")]
```

The lambda `\x -> lt x x` keeps `Ord a` in its scheme without mentioning `Eq a`, even though `lt` belongs to `Ord` and `Ord` declares `Eq` as a superclass. The reduction step that eliminates `Eq a` from the kept context is `simplify_context`, which notices that an `Ord a` binder already in scope can produce an `Eq a` dictionary by projection. The second example exercises the projection at use: `eq x y` inside a body that has both predicates in its environment elaborates as `d2.__super_Ord__Eq.eq x y`, walking from the kept `Ord` dictionary through the precomputed superclass slot to the `eq` method.

## Defaults

A default method gives the class a fallback implementation that an instance may omit.

```rust
#![source_file!("type-classes/tests/06_defaults.fun")]
```

The class declares `eq` and `neq`, with a default for `neq` written in terms of `eq`. The instance for `Int` supplies only `eq`. When `neq 1 2` is elaborated, the dictionary record carries both methods, and the `neq` slot contains the default body specialised by replacing the class predicate's dictionary with the literal `self`. The elaborated snapshot makes this self-reference explicit: `neq = \x -> \y -> primNotBool (self.eq x y)`. The `self` is bound implicitly when the dictionary is used, which is the standard knot-tying trick from dictionary-passing elaboration.

## Generalisation

Let-generalisation is the standard HM step extended to qualified types. The predicates accumulated during inference are partitioned into those that get hoisted into the new scheme and those that flow out into the surrounding context.

```rust
#![source_file!("type-classes/tests/07_generalize.fun")]
```

The binding `let same = \x -> eq x x` generalises to `forall a. Eq a => a -> Bool`. Reusing the bound name in a fresh predicate context produces nested `DictAbs` blocks: the outer expression has its own dictionary binder, and `same` itself elaborates with a dictionary binder, with the outer binder threaded in by `@`. The final example shows the principal scheme of a higher-order function `\f -> \x -> eq (f x) x` is `(a -> a) -> a -> Bool` with `Eq a` as its only predicate, because the only equality required of the input is on the result of `f` and on `x`, which share the same type variable.

## Multiple Classes

A single binding can carry predicates from several different classes when its body uses methods from each.

```rust
#![source_file!("type-classes/tests/08_multi_class.fun")]
```

The expression `\x -> primAndBool (eq x x) (eq (show x) x)` uses `eq` from `Eq` and `show` from `Show`, both at the same variable. The principal scheme `(Eq a, Show a) => a -> Bool` records both predicates because neither class reduces against the other. The elaborated form takes two dictionary binders and threads each method through its own dictionary, demonstrating that the context list maps directly onto the binder list of the outermost `DictAbs`.

## Errors

The error file pins the diagnostics for ill-typed programs.

```rust
#![source_file!("type-classes/tests/09_errors.fun")]
```

The first failure is `No instance for Eq Bool`, which fires because no `instance Eq Bool` is in scope when the resolver tries to discharge the predicate at the ground type. The second is `Unknown class 'Show'`, which fires when an instance declaration names a class that has not been declared. The third is `Unknown method 'neq'`, which fires when an instance binds a name that the class did not declare. Each path goes through `InferenceError` and is reported with the offending predicate or name attached.
