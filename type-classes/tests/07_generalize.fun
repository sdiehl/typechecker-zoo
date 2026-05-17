# Let-generalisation produces a qualified scheme. The bound name carries
# its dictionary abstraction internally.

class Eq a where { eq : a -> a -> Bool }

# A polymorphic helper picks up the Eq predicate as part of its scheme.
let same = \x -> eq x x

# Reusing the bound name at a fresh point still requires the predicate.
let same = \x -> eq x x in \y -> same y

# Generalisation is over qualified types: the predicate stays in the scheme.
\f -> \x -> eq (f x) x
