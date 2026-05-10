# Type errors. Each line should produce an error rather than a type.

# Selection of a label not present in a closed record.
{x = 1}.y
{x = 1, y = 2}.z

# Selection from the empty record.
{}.x

# Restriction of a label not present.
{{x = 1} - y}
{{} - x}

# Use a row-polymorphic projection at two incompatible field types.
let getx = \r -> r.x in let f = getx {x = 1} in getx {x = true}

# Force unification of two row tails carrying conflicting field types for x.
\r -> \k -> let a = {x = 1 | r} in let b = {x = true | r} in k a b

# Genuine Wand-style trigger for the (uni-row) side condition: the same `k`
# is applied to two records that share row tail `r` but disagree on the head
# label, forcing {x : Int | r} ~ {y : Int | r}. The side condition catches
# this as a recursive row binding rather than diverging.
\r -> \k -> let dummy = k {x = 1 | r} in k {y = 2 | r}

# Selection on a non-record (an Int).
(\x -> x.foo) 1

# Applying a record where a function is expected.
{x = 1} 2

# Extending a non-record.
{l = 1 | 42}

# Occurs check on type variables (classic self-application).
\x -> x x

# Mismatched arrow vs record.
(\f -> f 1) {x = 1}

# Unifying two closed records of different shapes (no tuple support; use sequencing).
let f = \r -> r.x in let p = {x = 1} in let q = {y = 2} in f q
