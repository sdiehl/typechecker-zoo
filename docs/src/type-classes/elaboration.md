# Dictionary Elaboration

Type class inference is not only a checking pass. It is also a translation. Each source program with abstract qualified types compiles into a core program in which every class predicate has been resolved to an explicit dictionary, and every overloaded method call has been replaced with a field projection out of that dictionary. The translation is the standard one introduced by the authors of _How to make ad-hoc polymorphism less ad hoc_, and the elaborated language is a System F-like calculus extended with dictionary records and operations on them.

## The Core Calculus

The core language extends the lambda calculus with four constructors that have no source-level counterpart.

```rust
#![enum!("type-classes/src/ast.rs", Core)]
```

`DictAbs` binds a list of dictionary variables, each tagged with the predicate it witnesses, and is the elaborated form of a let-binding whose scheme has a non-empty context. `DictApp` applies a function to a list of dictionary arguments, and is what every reference to a qualified scheme elaborates into. `DictProj` selects a field out of a dictionary record, which is how class methods are dispatched after elaboration. `DictRec` is a dictionary literal: a record of method implementations together with the class and instance type they are built for. The pretty printer uses an `@d` syntax for dictionary application and an angle-bracket syntax for dictionary records, so the elaborated output makes the dictionary structure visible at a glance.

## Method Dispatch

A reference to a class method elaborates into a projection out of a dictionary variable, which is itself bound somewhere in the surrounding context. The variable case of inference is the rule that performs this rewrite.

```rust
#![function_body!("type-classes/src/infer.rs", Checker::infer_var)]
```

When the looked-up name resolves to a method declared by some class `C`, the elaborator finds the freshly introduced dictionary variable carrying the predicate `C a` and emits `Core::DictProj(Var(d), method_name)`. Every later mention of that method in the same scheme reuses the same dictionary variable. When the name is an ordinary qualified binding, the elaborator emits `Core::DictApp(Var(name), [Var(d_0), ..., Var(d_n)])`, threading the same fresh dictionary variables in as type-class arguments.

## Dictionary Abstraction

When a let-binding generalises, the kept predicates become explicit binders in the elaborated body. The fragment from `generalize_binding` that constructs the binder list is the connection between the scheme's context and the elaborated term.

```rust
#![function!("type-classes/src/infer.rs", subst_core)]
```

Before the dictionary abstraction is wrapped around the body, every dictionary variable that has been discharged by `reduce` is substituted with the core proof returned by resolution. This is what eliminates non-head-normal predicates from the elaborated output: the predicate `Eq (List Int)` never reaches a `DictAbs` because resolution produces its proof during context reduction, and the proof flows through `subst_core` into the body. Only predicates that survived context reduction become binders, and only those binders survive into the elaborated form of the let-binding.

The snapshot for `07_generalize.fun` makes the structure plain.

```text
let same = \x -> eq x x in \y -> same y : Eq a => a -> Bool
  ~> \(d2 : Eq a) -> let same = \(d1 : Eq a) -> \x -> d1.eq x x in \y -> same @d2 y
```

The body of `same` carries its own dictionary binder `d1`, because the let-binding generalises and the predicate `Eq a` is part of its scheme. The outer expression is itself qualified, so the whole result is wrapped in a fresh `d2` binder and the reference to `same` becomes `same @d2`, dictionary-applying the generalised function to the outer scope's dictionary. This is what makes the elaborated language closed: every dictionary variable is bound by exactly one `DictAbs`, and every reference to a qualified scheme is mediated by `DictApp`.

## Instance Compilation

When an instance is declared, the elaborator type-checks each method body against the instance type, resolves any predicates that arise in those bodies against the instance's own context, and stores the resulting list of method cores as a record alongside the instance head.

```rust
#![function!("type-classes/src/infer.rs", Checker::process_instance_decl)]
```

The procedure substitutes the class's type parameter with the instance's head type, type-checks each method body at the specialised signature, and runs `reduce` against the instance's context as the local givens. The methods that survive become the body of the instance dictionary that gets emitted at every call site that resolves against this instance. Default methods declared in the class are filled in for instances that omit them, with the projection `self.method` standing in for any predicate that mentions the instance's class.

After the method records are built, the elaborator computes a superclass dictionary for each declared superclass and stores it under the slot name produced by `super_field`. This is what justifies eager superclass projection during resolution. By the time the instance is in the class environment, every declared superclass has its dictionary precomputed, so projecting an `Eq` dictionary out of an `Ord` dictionary is a constant-time field access rather than another resolution call.

The snapshot for `04_resolution.fun` shows what a use site looks like after elaboration.

```text
eq 1 2 : Bool
  ~> <Eq Int: eq = \x -> \y -> primEqInt x y>.eq 1 2
```

The angle-bracket form is a `DictRec`. The class name and instance type appear in the header, the method implementations appear in the body. The whole record is then projected on `eq` to recover the underlying function, which is finally applied to its two arguments. The application is fully ground and could be reduced by a beta-reducer that knows about `DictRec` and `DictProj`, which is the operational read of the elaborated language.

## Superclass Projection

Superclass projection is what makes `simplify_context` legal at the elaborated level. When the kept predicates contain both `Eq a` and `Ord a`, the `Eq a` binder is removed because its dictionary is recoverable as `d_Ord.__super_Ord__Eq`. The substitution map built by `simplify_context` records this rewrite, and `subst_core` walks the elaborated body replacing every reference to the dropped dictionary variable with the projection.

The snapshot for `05_superclass.fun` shows the projection at work in a body that uses both `eq` and `lt`.

```text
let cmp = \x -> \y -> primAndBool (eq x y) (lt x y) in cmp : Ord a => a -> a -> Bool
  ~> \(d3 : Ord a) -> let cmp = \(d2 : Ord a) -> \x -> \y -> primAndBool (d2.__super_Ord__Eq.eq x y) (d2.lt x y) in cmp @d3
```

The scheme carries only `Ord a` in its context because the `Eq a` predicate was discharged at the elaborated level by superclass projection. The reference to `eq` becomes `d2.__super_Ord__Eq.eq`, projecting first the `Eq` dictionary out of the `Ord` dictionary and then the `eq` method out of the `Eq` dictionary. The reference to `lt` projects directly, since `Ord` is its declaring class. The same `d2` binder serves both methods, which is what makes the context reduction sound: any method reachable through declared superclasses from a kept binder is reachable through `DictProj` chains in the elaborated form.
