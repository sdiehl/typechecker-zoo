# Basic literals
42
true
false

# Type annotations
\x : Int -> x
\x : Bool -> x
(\x : Int -> x) 42
(\x : Bool -> x) true

# Polymorphic identity using forall
forall a. \x : a -> x

# Type application with brackets
(forall a. \x : a -> x) [Int]
(forall a. \x : a -> x) [Bool]

# Higher-rank polymorphism examples  
\f : (forall a. a -> a) -> f
(\f : (forall a. a -> a) -> f) (forall a. \x : a -> x)

# Nested type abstractions
forall a. forall b. \x : a -> \y : b -> x
forall a. forall b. \x : a -> \y : b -> y

# Complex polymorphic examples
forall a. \f : (a -> a) -> \x : a -> f x

# Function types
\f : (Int -> Int) -> \x : Int -> f x
(\f : (Int -> Int) -> \x : Int -> f x) (\y : Int -> y) 42

# Error cases should be handled by the type checker
(\x : Int -> x) true