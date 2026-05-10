# Identity is fully polymorphic in type and effect
let id = \x -> x in id

# Pure let-bindings generalize and are reusable at multiple types
let id = \x -> x in let _ = id 1 in id true

# K combinator
let const = \x -> \y -> x in const

# Function composition stays pure
let compose = \f -> \g -> \x -> f (g x) in compose
