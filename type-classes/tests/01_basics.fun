# Underlying Hindley-Milner still works without any classes.

# The identity function generalises to forall a. a -> a.
let id = \x -> x

# Constant function.
let const = \x -> \y -> x

# A monomorphic int and bool literal.
42
true

# Polymorphic identity applied at two types in one expression.
let id = \x -> x in id true
