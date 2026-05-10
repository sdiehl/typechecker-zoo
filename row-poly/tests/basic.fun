# Basic literals and HM
42
true
\x -> x
\f -> \x -> f x

# Empty record
{}

# Closed records
{x = 1}
{x = 1, y = true}
{x = 1, y = 2, z = 3}

# Selection
{x = 1, y = true}.x
{x = 1, y = true}.y
\r -> r.x
\p -> p.x

# Polymorphic extension
\r -> {z = 0 | r}
\r -> {a = true, b = false | r}

# Selection through extension
let p = {x = 1, y = true} in p.x
let p = {x = 1, y = true} in p.y

# Restriction
{{x = 1, y = true} - x}
{{x = 1, y = true} - y}

# Update sugar (l := v | r) reduces to (l = v | (r - l))
\r -> {x := 0 | r}
let p = {x = 1, y = 2} in {x := 99 | p}

# Scoped labels: duplicate labels keep their order
{x = 1, x = true}
{x = 1, x = true}.x
{{x = 1, x = true} - x}.x

# Free extension over a polymorphic record
let extend_z = \r -> {z = 0 | r} in extend_z {x = 1, y = 2}

# Distance-style polymorphism
let distance = \p -> p.x in distance {x = 5, y = 7}

# Restriction in a function
\r -> {r - x}

# The classic non-terminating example terminates with the side condition
\r -> let f = \c -> {x = 1 | r} in let g = \c -> {y = 2 | r} in f

# Errors
{x = 1}.y
{}.x
\x -> x.foo
