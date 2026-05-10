# Selection in all its forms. Selection's principal type is
#   (.l) : forall r a. {l : a | r} -> a

# Direct selection from a literal.
{x = 42}.x

# Selection through let.
let p = {x = 1, y = 2} in p.x
let p = {x = 1, y = 2} in {a = p.x, b = p.y}

# Selection inside a lambda forces a row-polymorphic type.
\r -> r.x
\r -> r.foo

# Two selections from the same row reuse the row variable.
\r -> \s -> r.x

# Two selections from different records are independent.
\r -> \s -> r.x

# Polymorphic selector functions.
let getx = \r -> r.x in getx {x = 1}
let getx = \r -> r.x in getx {x = true}
let getx = \r -> r.x in {a = getx {x = 1, y = 2}, b = getx {x = false, z = 3}}

# Selection on extension.
({x = 1 | {y = 2}}).x
({x = 1 | {y = 2}}).y

# Function returning a selector.
\l -> l.field

# Higher-order: pass selector as argument.
\f -> \r -> f r

# Compose selectors via let.
let getx = \r -> r.x in let gety = \r -> r.y in \r -> getx r
