# Pure values have empty effect rows
1
true
()
\x -> x
\x -> \y -> x

# Pure functions are still pure
let id = \x -> x in id
let const = \x -> \y -> x in const

# Application of pure functions stays pure
(\x -> x) 1
let id = \x -> x in id true
