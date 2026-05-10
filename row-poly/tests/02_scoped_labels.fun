# Duplicate labels are allowed and form a stack.
# Selection and restriction always operate on the leftmost occurrence.

# Duplicate label, same type.
{x = 1, x = 2}

# Duplicate label, different types.
{x = 1, x = true}

# Three-deep stack.
{x = 1, x = true, x = false}

# Selection picks the leftmost.
{x = 1, x = true}.x
{x = true, x = 1}.x
{x = 1, x = true, x = false}.x

# Restriction peels off the leftmost, exposing the next.
{{x = 1, x = true} - x}.x
{{x = true, x = 1} - x}.x
{{{x = 1, x = true, x = false} - x} - x}.x

# Free extension can introduce a duplicate.
let p = {x = 1} in {x = true | p}
let p = {x = 1} in ({x = true | p}).x
let p = {x = 1} in ({{x = true | p} - x}).x

# Scoped labels inside nested records.
{outer = {x = 1, x = true}}
{outer = {x = 1, x = true}}.outer.x

# Extension that shadows a polymorphic field type.
\r -> {x = 1 | r}
\r -> ({x = 1 | r}).x
