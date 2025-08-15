# Basic literals
42
true
false

# Variables and identity
\x -> x
\f -> f
\x -> \y -> x
\x -> \y -> y

# Application
(\x -> x) 42
(\x -> x) true
(\f -> \x -> f x) (\y -> y) 42

# Let expressions
let x = 42 in x
let f = \x -> x in f
let id = \x -> x in id 42
let id = \x -> x in id true
let f = \x -> x in let g = \y -> y in f (g 42)

# Tuples
(42, true)
(true, false, 42)
(\x -> (x, x)) 42
let pair = \x -> \y -> (x, y) in pair 42 true

# Higher-order functions
\f -> \x -> f x
\f -> \x -> f (f x)
\f -> \g -> \x -> f (g x)
let twice = \f -> \x -> f (f x) in twice
let compose = \f -> \g -> \x -> f (g x) in compose

# Complex examples
let K = \x -> \y -> x in K
let S = \f -> \g -> \x -> f x (g x) in S
let Y = \f -> (\x -> f (x x)) (\x -> f (x x)) in Y

# Polymorphic examples (simplified for our basic implementation)
let id = \x -> x in (id, id)
let const = \x -> \y -> x in const
let flip = \f -> \x -> \y -> f y x in flip

# Error cases (should fail)
\x -> x x
(\x -> x x) (\x -> x x)