# Basic record construction and selection.

# The empty record.
{}

# A single field.
{x = 1}

# Multiple fields, mixed types.
{x = 1, y = true, z = false}

# Nested records.
{a = {b = 1}}
{a = {b = {c = 42}}}

# Chained selection.
{a = {b = 1}}.a
{a = {b = 1}}.a.b
{a = {b = {c = 42}}}.a.b.c

# Records inside a lambda body.
\x -> {self = x}
\x -> \y -> {a = x, b = y}

# Record let-binding then selection.
let p = {x = 1, y = 2} in p.x
let p = {x = 1, y = 2} in p.y

# Record fields holding functions.
{id = \x -> x}
{add = \x -> \y -> x, sub = \x -> \y -> y}
