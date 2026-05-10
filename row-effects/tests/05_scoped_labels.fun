# Scoped labels allow the same label to appear multiple times
# (same nesting as Leijen records: order matters, duplicates allowed)
let _ = perform print 1 in perform print 2

# Nested handlers strip labels one layer at a time
handle (handle perform throw 1 with throw x k -> 0) with throw x k -> 1

# A function performing two distinct effects has both in its row
\_ -> let _ = perform read () in perform tell 1
