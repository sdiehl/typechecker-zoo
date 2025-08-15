# Subtyping and Higher-Rank Polymorphism Edge Cases

# 1. Basic subtyping - monotypes
42
true
\x : Int -> x
\x : Bool -> x

# 2. Polymorphic identity should work with any type
(forall a. \x : a -> x) [Int] 42
(forall a. \x : a -> x) [Bool] true
(forall a. \x : a -> x) [(Int -> Int)] (\y : Int -> y)

# 3. Rank-2 polymorphism - functions accepting polymorphic functions
\k : (forall a. a -> a) -> k [Int] 42

# 4. Subtyping with existentials (System-F with existentials)
\f : (Int -> Int) -> f
\g : (forall a. a -> a) -> g

# 5. Complex nested applications that should work
(\p : (forall x. x -> x) -> p) (forall y. \z : y -> z)

# 6. Church encoding applications
(forall a. \t : a -> \f : a -> t) [Int] 42 0

# 7. Nested type abstractions applied step by step  
forall a. forall b. \x : a -> x
(forall a. forall b. \x : a -> x) [Int]
((forall a. forall b. \x : a -> x) [Int]) [Bool]

# 8. Testing instantiation with complex arrow types
(forall a. \x : a -> x) [(Bool -> Bool -> Bool)]

# 9. Higher-order polymorphic function composition
forall a. forall b. forall c. \f : (b -> c) -> \g : (a -> b) -> \x : a -> f (g x)

# 10. Apply polymorphic function multiple times
\h : (forall a. a -> a) -> \x : Int -> \y : Bool -> h [Int] (h [Bool] y)

# Edge cases that should produce type errors

# 11. Type mismatch - applying Int function to Bool
(\x : Int -> x) true

# 12. Trying to instantiate non-polymorphic function
(\x : Int -> x) [Bool]

# 13. Wrong number of arguments
(forall a. \x : a -> x) 42

# 14. Complex type mismatch in nested application
\f : (Int -> Int) -> \g : (Bool -> Bool) -> f g

# 15. Existential variable escape (should be caught)
\x : ^a -> x

# 16. Invalid type variable reference
\x : b -> x

# 17. Applying concrete function as if polymorphic
(\id : (Int -> Int) -> id [Bool]) (\x : Int -> x)

# 18. Complex rank-2 error case
\app : ((forall a. a -> a) -> Int) -> app (\x : Bool -> x)

# 19. Type annotation mismatch
(\x : Int -> x : Bool -> Bool)

# 20. Nested error propagation
(\bad : (forall a. a -> Int) -> bad [Bool] true) (\wrong : Bool -> wrong)