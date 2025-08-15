data Bool = True | False;

not :: Bool -> Bool;
not b = if b then False else True;

test :: Bool;
test = not True;