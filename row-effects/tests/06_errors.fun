# Unbound variable
foo

# Unknown effect operation
perform mystery 1

# Type mismatch in operation argument
perform print true

# Handler return type must match body type
handle perform read () with read x k -> true

# Type mismatch inside lambda body
(\x -> perform print x) true
