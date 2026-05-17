# Default methods supply a fallback definition in terms of others. The
# default is inserted into the dictionary when an instance omits the method.

class Eq a where { eq : a -> a -> Bool; neq : a -> a -> Bool; neq = \x -> \y -> primNotBool (eq x y) }

instance Eq Int where { eq = \x -> \y -> primEqInt x y }

# neq is reachable through the dictionary, dispatched to the default body.
neq 1 2

# Polymorphic uses of neq carry the same Eq predicate as eq does.
\x -> neq x x
