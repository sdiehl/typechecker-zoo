# Free extension: {l = e | r}. Always succeeds; never overwrites.
# Extension's principal type is
#   {l = _ | _} : forall r a. a -> {r} -> {l : a | r}

# Extension over the empty record.
{x = 1 | {}}
{x = 1, y = 2 | {}}

# Extension over a literal.
{z = 0 | {x = 1, y = 2}}

# Polymorphic extension.
\r -> {z = 0 | r}
\r -> {a = 1, b = 2, c = 3 | r}

# Iterated extension (literal sugar already does this).
{a = 1 | {b = 2 | {c = 3 | {}}}}

# Extension preserves the row variable for further use.
\r -> let s = {x = 1 | r} in s.x

# Extension that introduces a duplicate label is fine.
\r -> {x = true | {x = 1 | r}}

# Extension applied via let, used at multiple types.
let push = \r -> {z = 0 | r} in push {x = 1}
let push = \r -> {z = 0 | r} in push {y = true, w = false}

# A function that builds a closed record from two arguments.
\x -> \y -> {first = x, second = y}

# Extension and selection compose.
\r -> ({tag = 1 | r}).tag
\r -> ({tag = true | r}).tag

# Mixing extension with subsequent selection of original fields.
let make = \r -> {extra = 0 | r} in (make {x = 1}).x
