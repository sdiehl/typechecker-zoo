# Lambdas with refined argument types.
\x : { n : Int | n > 0 } -> x
\x : Int -> x + 1

# Dependent application with positive argument.
(\x : { n : Int | n > 0 } -> x) 3

# Built-in division by non-zero literal.
(\x : { n : Int | n != 0 } -> div 10 x) 2
