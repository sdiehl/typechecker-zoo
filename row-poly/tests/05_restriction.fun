# Restriction: {r - l}. Removes the leftmost l.
# Principal type: (- l) : forall r a. {l : a | r} -> {r}

# Restrict from a closed record.
{{x = 1, y = 2} - x}
{{x = 1, y = 2} - y}

# Restrict to empty.
{{x = 1} - x}

# Restrict from a polymorphic record.
\r -> {{x = 1 | r} - x}
\r -> {r - x}

# Restrict twice (must restrict different labels or rely on duplicates).
{{{x = 1, y = 2, z = 3} - x} - y}
{{{x = 1, y = 2, z = 3} - z} - x}

# Restrict the leftmost duplicate, then select the next.
{{x = 1, x = true} - x}.x
{{{x = 1, x = true, x = false} - x} - x}.x

# Restrict and then extend (the basis for update).
\r -> {x = true | {r - x}}

# Restrict from a let-bound polymorphic record.
let drop_x = \r -> {r - x} in drop_x {x = 1, y = 2}
let drop_x = \r -> {r - x} in drop_x {x = true, y = false, z = 99}

# Restriction commutes with selection on a different label.
\r -> ({r - x}).y
\r -> ({{x = 1 | r} - x}).y
