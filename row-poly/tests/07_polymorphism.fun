# Let-polymorphism over both type and row variables.

# A row-polymorphic function used at two different row instantiations.
let getx = \r -> r.x in {a = getx {x = 1}, b = getx {x = true}}

# Identity is row-polymorphic when applied to records.
let id = \x -> x in id {x = 1, y = true}
let id = \x -> x in {a = id {x = 1}, b = id {y = false, z = 0}}

# Polymorphism in both the field type and the row tail.
let getx = \r -> r.x in getx
let drop_x = \r -> {r - x} in drop_x

# Records of polymorphic functions: each field gets its own scheme on use.
let p = {id = \x -> x, fst = \x -> \y -> x} in p

# Records carrying functions used at multiple types.
let p = {id = \x -> x} in p

# Generalization respects free variables in the environment.
\r -> let getx = \s -> s.x in getx r

# Compose two row-polymorphic projections.
let getx = \r -> r.x in let gety = \r -> r.y in \r -> getx {dummy = gety r | r}

# Function whose type is forall r a. {x : a | r} -> a.
\r -> r.x

# Two independent row variables in a single signature.
\r -> \s -> {a = r.x, b = s.y}

# Higher-rank-flavored use: a record selector returned from let.
let mk = \k -> \r -> r in mk 0 {x = 1}
