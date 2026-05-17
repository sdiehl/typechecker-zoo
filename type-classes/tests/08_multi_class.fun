# A binding may carry multiple class predicates that do not reduce against
# each other. The scheme records all of them in its qualified context.

class Eq a where { eq : a -> a -> Bool }
class Show a where { show : a -> a }

# Two independent classes both touching the same variable.
\x -> primAndBool (eq x x) (eq (show x) x)
