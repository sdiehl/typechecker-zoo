# Higher-order combinators built around records.

# Function taking a record argument.
\p -> p.x
\p -> {x = p.x, y = p.y}

# Function returning a record.
\x -> \y -> {fst = x, snd = y}

# Apply a function to a selected field.
\f -> \r -> f r.x

# Compose two functions.
let compose = \f -> \g -> \x -> f (g x) in compose

# Compose then project.
let compose = \f -> \g -> \x -> f (g x) in let getx = \r -> r.x in compose getx (\v -> {x = v})

# A "modify" combinator that runs a function over a single field.
\f -> \r -> {x := f r.x | r}

# Building a getter/setter pair.
let getter = \r -> r.x in let setter = \v -> \r -> {x := v | r} in {get = getter, set = setter}

# Curried record builder.
\x -> \y -> \z -> {a = x, b = y, c = z}

# Pipeline-like: extend, then select.
\r -> ({extra = 0 | r}).extra

# Function taking a record of two fields and returning a swap.
\p -> {x = p.y, y = p.x}

# Apply a polymorphic function inside a record literal.
let id = \x -> x in {a = id 1, b = id true, c = id (\x -> x)}
