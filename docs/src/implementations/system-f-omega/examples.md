# Examples

Our System F-ω implementation demonstrates its capabilities through a comprehensive suite of working examples that showcase the full range of the type system's features. These examples progress from basic algebraic data types through  higher-order polymorphic functions, illustrating how System F-ω enables advanced programming patterns while maintaining type safety.

The examples serve both as demonstrations of the implementation's correctness and as practical illustrations of how System F-ω's theoretical power translates into useful programming language features. Each example successfully type checks under our implementation, proving that the  algorithms can handle real-world programming scenarios.

## Basic Data Types and Pattern Matching

The foundation of our System F-ω implementation lies in its support for algebraic data types with comprehensive pattern matching. These features provide the building blocks for more  programming patterns.

```rust
#![source_file!("system-f-omega/examples/final_demo.hs")]
```

### Algebraic Data Type Declarations

The implementation supports rich data type definitions that demonstrate System F-ω's kind system:

* **Simple Enumerations**: `data Bool = True | False` creates a basic sum type
* **Parameterized Types**: `data Maybe a = Nothing | Just a` shows kind `* -> *`
* **Multi-Parameter Types**: `data Either a b = Left a | Right b` demonstrates kind `* -> * -> *`
* **Recursive Types**: `data List a = Nil | Cons a (List a)` enables inductive data structures

Each declaration automatically infers appropriate kinds for the type constructors, showing how the implementation handles the kind system transparently.

### Pattern Matching with Type Safety

Pattern matching provides safe destructuring of algebraic data types:

```haskell
not :: Bool -> Bool;
not b = match b {
  True -> False;
  False -> True;
};

isJust :: Maybe a -> Bool;
isJust m = match m {
  Nothing -> False;
  Just x -> True;
};
```

The type checker verifies that:
- All patterns cover the correct constructors
- Pattern variables receive appropriate types
- Branch expressions have compatible return types
- Polymorphic types are handled consistently across branches

## Polymorphic Functions

System F-ω's universal quantification enables functions that work uniformly across all types, demonstrating parametric polymorphism in action.

### Basic Polymorphic Functions

```haskell
id :: a -> a;
id x = x;

const :: a -> b -> a;
const x y = x;
```

These functions showcase:
- **Type Variable Scope**: Variables like `a` and `b` are implicitly quantified
- **Principal Types**: The implementation infers the most general possible types
- **Polymorphic Instantiation**: Each use can instantiate types differently

### Higher-Order Polymorphic Functions

```haskell
map :: (a -> b) -> List a -> List b;
map f lst = match lst {
  Nil -> Nil;
  Cons x xs -> Cons (f x) (map f xs);
};

fromMaybe :: a -> Maybe a -> a;
fromMaybe def m = match m {
  Nothing -> def;
  Just x -> x;
};
```

These examples demonstrate:
- **Function Types as Arguments**: `(a -> b)` shows higher-order typing
- **Recursive Polymorphic Functions**: `map` calls itself with consistent types
- **Type-Safe Default Values**: `fromMaybe` maintains type consistency

## Complex Polymorphic Programming

More  examples show how System F-ω handles complex interactions between polymorphism, higher-order functions, and algebraic data types.

### Arithmetic and Comparison Operations

```haskell
add :: Int -> Int -> Int;
add x y = x + y;

multiply :: Int -> Int -> Int;
multiply x y = x * y;

lessThan :: Int -> Int -> Bool;
lessThan x y = x < y;
```

Built-in operations integrate seamlessly with the user-defined type system, showing how primitive types participate in the same type-theoretic framework as algebraic data types.

### Function Composition and Application

```haskell
composed :: Int;
composed = add (multiply 6 7) 8;

listLength :: List a -> Int;
listLength lst = match lst {
  Nil -> 0;
  Cons x xs -> add 1 (listLength xs);
};
```

These examples demonstrate:
- **Nested Function Applications**: Complex expressions type check correctly
- **Polymorphic Recursion**: `listLength` works for lists of any type
- **Type Preservation**: All intermediate computations maintain type safety

## Advanced Programming Patterns

Our implementation handles  programming patterns that require the full power of System F-ω's type system.

### Constructor Applications and Type Inference

```haskell
testBool :: Bool;
testBool = not True;

testMaybe :: Maybe Int;
testMaybe = Just 42;

testList :: List Int;
testList = Cons 1 (Cons 2 (Cons 3 Nil));
```

The type checker correctly infers types for constructor applications, handling:
- **Type Application**: `Just` applied to `42` infers `Maybe Int`
- **Nested Constructors**: Complex list structure maintains type consistency
- **Polymorphic Instantiation**: Each constructor use gets appropriate type arguments

### Pattern Matching with Complex Types

```haskell
either :: (a -> c) -> (b -> c) -> Either a b -> c;
either f g e = match e {
  Left x -> f x;
  Right y -> g y;
};

mapMaybe :: (a -> b) -> Maybe a -> Maybe b;
mapMaybe f m = match m {
  Nothing -> Nothing;
  Just x -> Just (f x);
};
```

These functions showcase:
- **Higher-Order Pattern Matching**: Functions as arguments in pattern contexts
- **Type-Safe Elimination**: Pattern matching preserves all type relationships
- **Functor Patterns**: `mapMaybe` demonstrates structure-preserving transformations

## Working Example Programs

The implementation includes several complete programs that demonstrate all features working together:

### Fibonacci with Polymorphic Utilities

One example program implements Fibonacci numbers using polymorphic helper functions, showing how System F-ω enables code reuse:

```haskell
fibonacci :: Int -> Int;
fibonacci n = match lessThan n 2 {
  True -> n;
  False -> add (fibonacci (subtract n 1)) (fibonacci (subtract n 2));
};
```

### List Processing with Higher-Order Functions

Another example demonstrates functional programming patterns with lists:

```haskell
filter :: (a -> Bool) -> List a -> List a;
filter pred lst = match lst {
  Nil -> Nil;
  Cons x xs -> match pred x {
    True -> Cons x (filter pred xs);
    False -> filter pred xs;
  };
};

foldRight :: (a -> b -> b) -> b -> List a -> b;
foldRight f acc lst = match lst {
  Nil -> acc;
  Cons x xs -> f x (foldRight f acc xs);
};
```

## Type Inference in Action

The examples demonstrate how the DK worklist algorithm handles complex type inference scenarios:

### Existential Variable Resolution

When processing expressions like `map (add 1) someList`, the algorithm:
1. **Generates existential variables** for unknown types
2. **Propagates constraints** through function applications
3. **Unifies types** to discover that the list must have type `List Int`
4. **Reports the final type** as `List Int`

### Polymorphic Instantiation

For expressions using polymorphic functions multiple times:
```haskell
example = (id True, id 42, id someList)
```

The algorithm correctly instantiates `id` with different types:
- `id :: Bool -> Bool` for the first component
- `id :: Int -> Int` for the second component
- `id :: List a -> List a` for the third component

### Higher-Rank Polymorphism

The implementation handles functions that accept polymorphic arguments:
```haskell
applyToEach :: (forall a. a -> a) -> (Bool, Int) -> (Bool, Int);
applyToEach f (x, y) = (f x, f y);
```

This demonstrates the implementation's support for higher-rank types where polymorphic types appear in argument positions.
