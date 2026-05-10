# Termination corner cases for row unification.
# The side condition `tail(r) ∉ dom(θ1)` in (uni-row) prevents these from
# diverging (Leijen 2005, sect. 7.1).

# The classic non-terminating example from Wand. Both branches share the row
# variable r. Without the side condition, unifying {x : Int | r} ~ {y : Int | r}
# would loop. With it, inference completes and the function is row-polymorphic
# in r but contributes both labels to the result.
\r -> let f = \c -> {x = 1 | r} in let g = \c -> {y = 2 | r} in f

# Same idea, three different labels.
\r -> let f = \c -> {x = 1 | r} in let g = \c -> {y = 2 | r} in let h = \c -> {z = 3 | r} in f

# Common-tail unification with field types that must agree.
\r -> let mk = \c -> {x = c | r} in mk

# Two records over the same tail, used independently.
\r -> let a = {x = 1 | r} in let b = {y = true | r} in a

# Adding fields one-by-one.
\r -> {a = 1 | {b = 2 | {c = 3 | r}}}

# Open record in, open record out, types fully inferred.
\r -> {x := r.x | r}

# Selection from a row variable (infers a row-polymorphic record).
\r -> r.field

# Round-trip: restrict then extend with a fresh value of the same label.
\r -> {x = 0 | {r - x}}

# Two rows of unknown shape, unified by selection.
\r -> \s -> let pair = {a = r.x, b = s.x} in pair

# A function whose argument record must contain at least three labels.
\r -> {p = r.x, q = r.y, s = r.z}
