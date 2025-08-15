# Classic System-F Edge Cases

# 1. Higher-rank polymorphism - the classic challenge
# Functions that take polymorphic arguments
\f : (forall a. a -> a) -> f [Int] 42

# 2. Rank-2 polymorphism - function that applies polymorphic function
\g : (forall a. a -> a) -> \x : Int -> g [Int] x

# 3. Church encodings with polymorphism
# Boolean encoding - true selector
forall a. \t : a -> \f : a -> t

# Boolean encoding - false selector  
forall a. \t : a -> \f : a -> f

# Church numerals (zero)
forall a. \f : (a -> a) -> \x : a -> x

# Church numerals (one)
forall a. \f : (a -> a) -> \x : a -> f x

# Church numerals (two)
forall a. \f : (a -> a) -> \x : a -> f (f x)

# 4. Identity function instantiation edge cases
# Should work: instantiate polymorphic id with different types
((forall a. \x : a -> x) [(Int -> Int)]) (\y : Int -> y)

# 5. Nested quantification ordering
forall a. forall b. \x : a -> \y : b -> x

# 6. Type application with complex types
(forall a. \x : a -> x) [(forall b. b -> b)]

# 7. Higher-order polymorphic functions
forall a. forall b. \f : (a -> b) -> \g : (forall c. c -> a) -> \x : b -> f (g [a] x)

# 8. Contravariance in function arguments
\f : (forall a. a -> a) -> \g : (Int -> Int) -> f g

# 9. Complex nested application
\h : (forall a. a -> a) -> h (forall b. \z : b -> z) 

# 10. Predicative vs impredicative instantiation edge case
forall a. \x : (a -> a) -> x

# 11. Type abstraction with dependency
forall a. \f : (a -> a) -> forall b. \g : (b -> a) -> \x : b -> f (g x)

# 12. Multiple type applications in sequence
((forall a. forall b. \x : a -> \y : b -> x) [Int]) [Bool]

# 13. Rank-N polymorphism examples
\k : (forall a. forall b. a -> b -> a) -> k

# 14. Polymorphic Church pair constructor
forall a. forall b. forall c. \x : a -> \y : b -> \f : (a -> b -> c) -> f x y

# 15. System-F specific: type abstraction in argument position
\poly : (forall a. a -> a) -> poly [Int]

# 16. Nested polymorphic function application
(\id : (forall a. a -> a) -> id [Bool] true) (forall x. \v : x -> v)

# 17. Complex instantiation chain
(((forall a. forall b. forall c. \f : (a -> b -> c) -> f) [Int]) [Bool]) [Int]

# Error cases that should be caught by bidirectional type checking

# 18. Type mismatch in application
(\x : Int -> x) true

# 19. Applying non-polymorphic function as if it were polymorphic
(\x : Int -> x) [Bool]

# 20. Wrong type in polymorphic context
\f : (forall a. a -> a) -> f 42

# 21. Invalid type instantiation - applying concrete to concrete
(forall a. \x : a -> \y : a -> x) [Int] 42 true

# 22. Attempting to use existential directly (should fail in pure System-F)
\x : (^a) -> x