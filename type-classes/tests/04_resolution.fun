# Instance resolution at ground types: the dictionary is built at the use site.

class Eq a where { eq : a -> a -> Bool }
instance Eq Int where { eq = \x -> \y -> primEqInt x y }
instance Eq Bool where { eq = \x -> \y -> primEqBool x y }

# Each call site picks its instance independently.
eq 1 2
eq true false

# Predicates from polymorphic uses still resolve when applied at a ground type.
let test = \x -> eq x x in test 42
