# Single perform produces a singleton effect row
perform print 1
perform read ()
perform throw 1
perform ask ()
perform tell 5

# Sequencing operations accumulates labels in scoped order
let x = perform read () in perform print x
let _ = perform tell 1 in perform tell 2

# A function whose body performs an effect is annotated on the arrow
\x -> perform print x
\_ -> perform read ()
