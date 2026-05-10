# Update: {l := e | r} desugars to {l = e | {r - l}}.
# Because of that desugaring, update can change a field's type.

# Update preserving the type.
let p = {x = 1, y = 2} in {x := 99 | p}

# Update changing the type (heterogeneous update).
let p = {x = 1, y = 2} in {x := true | p}

# Update on a polymorphic record.
\r -> {x := 0 | r}
\r -> {x := true | r}

# Update twice on the same label.
let p = {x = 1} in {x := false | {x := true | p}}

# Update preserves the rest of the row.
let p = {x = 1, y = 2, z = 3} in ({x := true | p}).y
let p = {x = 1, y = 2, z = 3} in ({x := true | p}).z

# Update on a duplicate field hits the leftmost (the duplicate stays).
let p = {x = 1, x = true} in {x := false | p}
let p = {x = 1, x = true} in ({x := false | p}).x

# Updating creates a record whose row is open if the source is open.
\r -> ({x := 1 | r}).x

# Building an updater function.
let setx = \v -> \r -> {x := v | r} in setx 42 {x = 1, y = 2}
let setx = \v -> \r -> {x := v | r} in setx true {x = 1}
