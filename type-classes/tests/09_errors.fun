# Error paths. Each line below should be rejected by the checker.

class Eq a where { eq : a -> a -> Bool }
instance Eq Int where { eq = \x -> \y -> primEqInt x y }

# Bool has no Eq instance in this fragment.
eq true false

# Unknown class.
instance Show Int where { show = \x -> x }

# Method not declared by the class.
instance Eq Bool where { neq = \x -> \y -> false }
