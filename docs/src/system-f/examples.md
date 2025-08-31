# Examples

System F's power lies in its ability to express polymorphic computations that work across different types while maintaining type safety. This chapter explores a range of examples that demonstrate the expressive capabilities of our implementation, from simple polymorphic functions to complex higher-rank polymorphism scenarios.

Our examples are drawn from real test cases in the implementation, showing both the syntax our parser accepts and the types our bidirectional algorithm infers. These examples illustrate the key features of System F and demonstrate how the type system guides program construction.

## Basic Polymorphic Functions

The fundamental building block of System F is the polymorphic identity function. This simple example demonstrates universal quantification and type application in their most basic form.

Let's examine how our implementation handles the core examples from the test suite:

```rust
#![source_file!("system-f/tests/basic.fun")]
```

These test cases reveal several important aspects of our System F implementation:

### Literal Types and Basic Functions

The simplest cases involve literal values and monomorphic functions, where our bidirectional algorithm produces clean, precise types:

- `42 : Int` - Integer literals immediately receive the base type `Int`
- `true : Bool` and `false : Bool` - Boolean literals similarly receive their base types
- `\x : Int -> x : Int -> Int` - Monomorphic lambda expressions receive concrete function types

The type inference algorithm resolves all constraints to produce the most specific types possible. When we provide explicit type annotations, the algorithm can determine the complete function type without ambiguity.

### Type Annotations and Explicit Typing

System F requires explicit type annotations on lambda parameters, and our bidirectional algorithm infers complete function types:

- `\x : Int -> x` produces the type `Int -> Int`
- `(\x : Int -> x) 42` applies this function to produce type `Int`

These examples show how the algorithm handles the interaction between explicit annotations and type inference. The parameter type is fixed by annotation, and the algorithm infers that the return type must match the parameter type since we're returning the parameter unchanged.

### Universal Quantification in Practice

The true power of System F emerges with universal quantification. The polymorphic identity function demonstrates this clearly:

- `forall a. \x : a -> x : ∀a. a -> a`

This example shows several important aspects of our implementation:

1. **Type Abstraction**: The `forall a.` syntax introduces a type variable into scope
2. **Polymorphic Parameters**: The parameter `x : a` uses the quantified type variable
3. **Complete Type Inference**: The algorithm correctly resolves all type constraints to produce the expected polymorphic type

### Type Application and Instantiation

Type application allows us to specialize polymorphic functions to specific types:

- `(forall a. \x : a -> x) [Int] : Int -> Int`
- `(forall a. \x : a -> x) [Bool] : Bool -> Bool`

Both applications produce the expected concrete function types, showing that our algorithm correctly instantiates the polymorphic function with the requested types and resolves all type constraints to their final forms.

## Higher-Rank Polymorphism

System F supports higher-rank polymorphism, where polymorphic types can appear in argument positions. This creates functions that accept polymorphic arguments:

- `\f : (forall a. a -> a) -> f : ^α0 -> ^α1`

This function accepts any argument of type `forall a. a -> a` (a polymorphic identity function) and returns it unchanged. The higher-rank nature means the caller must provide a function that works for *all* types, not just some specific type.

The application example demonstrates this in action:

- `(\f : (forall a. a -> a) -> f) (forall a. \x : a -> x) : ^α0`

Here we pass the polymorphic identity function to a function that expects exactly that type signature. This showcases the precision and expressiveness of System F's type system.

## Nested Type Abstractions

System F allows multiple type abstractions to be nested, creating functions that are polymorphic in multiple type parameters:

- `forall a. forall b. \x : a -> \y : b -> x : ∀a. ∀b. a -> b -> a`
- `forall a. forall b. \x : a -> \y : b -> y : ∀a. ∀b. a -> b -> b`

These examples create functions that:
1. Are polymorphic in two types `a` and `b`
2. Accept arguments of those respective types
3. Return either the first or second argument

The `K` and `S` combinators from combinatory logic can be expressed naturally in this style, showing System F's expressiveness for abstract computation. As a fun aside, you express every other function in terms of just `K` and `S`, although in practice this is more of an theoretical curio because its unimaginably inefficient.

## Complex Polymorphism

More examples demonstrate how System F handles complex combinations of polymorphism and function application:

- `forall a. \f : (a -> a) -> \x : a -> f x : ∀a. (a -> a) -> a -> a`

This creates a function that:
1. Is polymorphic in type `a`
2. Accepts a function `f : a -> a` (an endomorphism on `a`)
3. Accepts a value `x : a`
4. Applies the function to the value

This pattern is fundamental in functional programming and demonstrates how System F naturally expresses higher-order polymorphic functions.
