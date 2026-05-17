# Refinement obligations that should fail.

# Divide by zero literal.
div 10 0

# Pass possibly-zero where non-zero is required.
\x : Int -> div 10 x

# Subtype mismatch: zero where positive is required.
let y = 0 in (\x : { n : Int | n > 0 } -> x) y

# Pass negative where positive is required.
(\x : { n : Int | n > 0 } -> x) (0 - 5)
