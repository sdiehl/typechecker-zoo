# A class declaration introduces method names with qualified schemes.

class Eq a where { eq : a -> a -> Bool }

# Each method is now in scope at its qualified type.
eq

# Curried partial application keeps the class predicate.
\x -> eq x
