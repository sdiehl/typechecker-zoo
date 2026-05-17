# Ord extends Eq. The superclass dictionary is built when the instance is
# declared and stored in a hidden field of the subclass dictionary.

class Eq a where { eq : a -> a -> Bool }
class Eq a => Ord a where { lt : a -> a -> Bool }

instance Eq Int where { eq = \x -> \y -> primEqInt x y }
instance Ord Int where { lt = \x -> \y -> primLtInt x y }

# Using only Ord retains Ord; Eq is projected out when needed.
\x -> lt x x

# Mixing Eq and Ord predicates collapses to just Ord by context reduction.
let cmp = \x -> \y -> primAndBool (eq x y) (lt x y) in cmp
