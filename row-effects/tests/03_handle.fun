# Handling discharges the label from the body's effect row
handle perform throw 1 with throw x k -> 0
handle perform read () with read x k -> 42
handle perform tell 1 with tell x k -> ()

# Handler can resume with a value of the op's return type
handle perform read () with read x k -> k 7
handle perform print 1 with print x k -> k ()

# Pure body still goes through handle (label removed if present, otherwise irrelevant)
handle 1 with throw x k -> 0
