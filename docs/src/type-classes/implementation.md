# Implementation

## Types and Predicates

The type language is the small core of variables, type constructors with arguments, and arrows. Predicates are the new syntactic category that travels alongside types throughout inference.

```rust
#![enum!("type-classes/src/ast.rs", Type)]
```

The `Var` constructor wraps a type variable name, the workhorse of polymorphism. The `Con` constructor pairs a type constructor name with a vector of argument types and covers everything from nullary primitives like `Int` (encoded as `Con("Int", [])`) to parameterised constructors like `List Int` (encoded as `Con("List", [Con("Int", [])])`). The `Arr` constructor builds a function type from an argument and a result. Arrows are not encoded as a special two-argument constructor because the inference engine specialises on them, and the dedicated case keeps the substitution and unification code transparent.

```rust
#![struct!("type-classes/src/ast.rs", Pred)]
```

The `class` field is the name of the class being asserted and the `ty` field is the single type the assertion is about. A predicate `Eq Int` has class `"Eq"` and type `Type::int()`. A predicate `Ord (List a)` has class `"Ord"` and type `Type::Con("List", [Type::Var("a")])`. There is no separate notion of multi-parameter classes in this fragment, so the type field is always a single type rather than a vector.

A qualified type pairs a list of predicates with a head type.

```rust
#![struct!("type-classes/src/ast.rs", Qual)]
```

The `preds` field is the context of predicates that must be discharged before the term takes on its head type, and the `ty` field is the head type itself. The qualified type `Eq a => a -> a -> Bool` has predicates `[Eq a]` and head `a -> a -> Bool`. The empty context is the degenerate qualified type that is indistinguishable from a plain monotype.

A scheme quantifies over type variables with a qualified body.

```rust
#![struct!("type-classes/src/ast.rs", Scheme)]
```

The `vars` field is the list of universally quantified type variables and the `qual` field is the qualified body the quantifiers bind. The scheme `forall a. Eq a => a -> a -> Bool` is `Scheme { vars: ["a"], qual: Qual { preds: [Eq a], ty: a -> a -> Bool } }`. The pretty printer writes the empty context invisibly and the singleton context without parentheses, so a class with no predicates prints as ordinary HM.

## Inference

Inference is Algorithm W with one additional output: a list of pending predicates collected from each subterm. The return type is a quadruple of substitution, type, pending predicates, and an elaborated core term.

```rust
#![function!("type-classes/src/infer.rs", Checker::infer_expr)]
```

The variable case instantiates the bound scheme, replacing each universally quantified variable with a fresh type variable and each context predicate with a fresh dictionary variable. The list of fresh dictionary variables threaded through the result is the source of all pending predicates in the system.

```rust
#![function!("type-classes/src/infer.rs", Checker::infer_var)]
```

When the referenced name is a class method rather than an ordinary binding, the elaborated form is a projection out of the freshly introduced dictionary variable rather than a use of the source name. This is what justifies erasing the method's source identity entirely once elaboration is done. When the name is an ordinary qualified binding such as a generalised let-bound function, the elaborated form is an explicit dictionary application that supplies the fresh dictionary variables as type-class arguments.

Abstraction extends the environment with a monomorphic scheme for the parameter, infers the body, and assembles an arrow.

```rust
#![function!("type-classes/src/infer.rs", Checker::infer_abs)]
```

Predicates collected from the body pass through unchanged. The scheme for the parameter is monomorphic because Hindley-Milner does not generalise inside a lambda, so any predicate that arises inside the body must wait until at least the surrounding let-binding for a chance to be generalised or discharged.

Application is the standard rule with predicate union. The function and the argument both produce pending predicates, and the result inherits both.

```rust
#![function!("type-classes/src/infer.rs", Checker::infer_app)]
```

Let is where generalisation happens and where predicates are split between the new scheme and the deferred context.

```rust
#![function!("type-classes/src/infer.rs", Checker::infer_let)]
```

After inferring the body of the binding, the inference engine calls `generalize_binding` to compute the scheme, the elaborated core term wrapped in a dictionary abstraction over the kept predicates, and the deferred predicates that escape into the surrounding context. The body of the let is then inferred under the new scheme, and its predicates pass through to the result.

## Predicate Resolution

The class environment stores every declared class and every declared instance. Instance lookup is a structural match against the head pattern, and superclass projection walks the chain of declared superclasses.

```rust
#![function!("type-classes/src/classes.rs", by_inst)]
```

The function returns the list of subgoals and the index of the matching instance. Subgoals are the instance's context with the matcher substitution applied, which captures the standard Prolog-style resolution step: to prove `Eq (List a)`, look up the instance `Eq a => Eq (List a)`, substitute `a` for itself, and recurse to prove the subgoal `Eq a`.

Superclass projection is the dual: to prove `Eq a` from a hypothesis `Ord a`, project the hidden `Eq` field out of the `Ord` dictionary.

```rust
#![function!("type-classes/src/classes.rs", by_super)]
```

The function returns every predicate reachable from the starting hypothesis by following declared superclasses, paired with the corresponding core term that performs the projections. The name `super_field(class, sup)` is a deterministic mangling that picks out the slot for the superclass dictionary inside the subclass dictionary record. The chapter on [Dictionary Elaboration](./elaboration.md) explains how that slot is filled when the instance is declared.

The full resolver combines these two mechanisms.

```rust
#![function!("type-classes/src/classes.rs", resolve)]
```

The first pass tries to discharge the goal from the local givens, walking each given's superclass chain in case the goal sits behind a chain of declared superclass relationships. The second pass tries to match the goal against an instance head, and recursively resolves the subgoals that the instance's context introduces.

## Context Reduction

After inference, the pending predicates may contain duplicates, predicates that are implied by other predicates in the same context, and predicates that can be discharged against the instance database without waiting for a more concrete call site. Context reduction collapses the list before generalisation.

```rust
#![function!("type-classes/src/classes.rs", reduce)]
```

A predicate is in head-normal form when its type is a type variable rather than a constructor applied to arguments. Predicates not in head-normal form, such as `Eq (List a)`, cannot wait for a concrete instantiation because the head constructor is already concrete, so they are resolved immediately and their core proofs replace the dictionary variable in the elaborated term. Predicates in head-normal form, such as `Eq a`, may still be discharged against the local givens via superclass projection, but otherwise become part of the residual context that flows into the scheme.

Simplification removes predicates that are implied by others in the same context. The classic case is `(Eq a, Ord a) => ...` which collapses to `Ord a => ...` because the superclass declaration `class Eq a => Ord a` makes the `Eq` part recoverable from the `Ord` dictionary.

```rust
#![function!("type-classes/src/classes.rs", simplify_context)]
```

The function walks the context one binder at a time. For each binder it asks whether the predicate can be recovered as a superclass projection from any of the remaining binders. If yes, the binder is removed and its dictionary variable is rewritten to the projection. If no, the binder is kept.

## Generalisation

Generalisation builds the final scheme and the final elaborated body.

```rust
#![function!("type-classes/src/infer.rs", Checker::generalize_binding)]
```

The procedure runs in four passes. The first pass calls `reduce` to discharge non-head-normal predicates against the instance database. The second pass partitions the residual into those that touch the inferred type or escape into the surrounding environment, which are kept, and those that touch only variables already free in the environment, which are deferred. The third pass runs `simplify_context` to drop superclass-implied predicates from the kept list. The fourth pass builds a `DictAbs` wrapping the original core in a binder for each kept predicate, and assembles the final scheme with the kept predicates as its context and the kept type variables as its quantifiers.

The ambiguity check at the tail of the function rejects schemes that quantify over a type variable that appears only in a predicate and nowhere in the head type or the environment. The standard offender is `\f -> primAndBool (eq (f 1)) ...` where `f` has type `Int -> a` for some fresh `a`, the predicate `Eq a` is generated, and `a` appears nowhere in the head type so no call site could ever pick a witness. The error is the qualified-types analogue of an unsolvable metavariable.
