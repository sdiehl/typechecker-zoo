# A class plus one ground-type instance.

class Eq a where { eq : a -> a -> Bool }

# A primitive equality witness. The body uses an opaque primitive we treat
# as a free variable of the right type.
instance Eq Int where { eq = \x -> \y -> primEqInt x y }

# Using eq at a concrete type resolves to the instance dictionary.
\x -> eq x x

# Direct application discharges the predicate at the call site.
let f = \x -> eq x x in f
