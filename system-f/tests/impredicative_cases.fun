# Impredicative Polymorphism and Advanced System-F Cases

# 1. Classic impredicative polymorphism - polymorphic identity applied to itself
(forall a. \x : a -> x) [(forall b. b -> b)] (forall c. \y : c -> y)

# 2. Self-application of polymorphic functions (should work in System-F)
\self : (forall a. a -> a) -> self [forall b. b -> b] self

# 3. Polymorphic function taking polymorphic argument
\poly : (forall a. a -> a) -> \x : Int -> poly [Int] x

# 4. Church encoding of pairs with polymorphic components
forall a. forall b. \x : a -> \y : b -> forall c. \f : (a -> b -> c) -> f x y

# 5. Y combinator attempt (will likely fail due to occurs check)
\f : (Int -> Int) -> (\x : Int -> f (x x)) (\x : Int -> f (x x))

# 6. Rank-3 polymorphism example
\r3 : ((forall a. a -> a) -> Int -> Int) -> r3 (\id : (forall b. b -> b) -> id [Int])

# 7. Complex nested quantification
forall a. \f : (forall b. b -> a) -> forall c. f [c]

# 8. Polymorphic recursion simulation (without actual recursion)
forall a. \rec : (a -> a) -> \base : a -> rec base

# 9. Church boolean with polymorphic branches
\bool : (forall a. a -> a -> a) -> bool [(forall b. b -> b)]

# 10. Testing existential instantiation boundaries
\exist_test : (^a -> ^a) -> exist_test

# 11. Multiple nested applications with different quantifiers
((forall a. forall b. forall c. \f : (a -> b -> c) -> f) [Int]) [Bool] [\z : Int -> z] 42 true

# 12. Polymorphic composition chain
forall a. forall b. forall c. \f : (b -> c) -> \g : (a -> b) -> forall d. \h : (d -> a) -> \x : d -> f (g (h x))

# 13. Higher-kinded simulation (limited without kind system)
forall f. \wrap : f -> wrap

# 14. Complex Church numeral operations
\succ : (forall a. (a -> a) -> a -> a) -> forall b. \f : (b -> b) -> \x : b -> f (succ [b] f x)

# 15. Polymorphic conditionals
forall a. \cond : Bool -> \then : a -> \else : a -> then

# Error cases for impredicative polymorphism

# 16. Invalid self-reference
\bad : (a -> a) -> bad bad

# 17. Type variable escape in impredicative context  
forall a. \escape : (a -> forall a. a) -> escape

# 18. Occurs check with polymorphic types
\occurs : (forall a. a -> forall a. a -> a) -> occurs occurs

# 19. Invalid quantifier nesting
\nested_bad : (forall a. forall a. a -> a) -> nested_bad

# 20. Complex subtyping failure
\sub_fail : ((forall a. a -> Int) -> Bool) -> sub_fail (\x : Bool -> 42)