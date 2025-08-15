-- Test of currently working features

-- Basic ADTs work perfectly
data Bool = True | False;
data Maybe a = Nothing | Just a;
data Either a b = Left a | Right b;

-- Pattern matching works
not :: Bool -> Bool;
not b = match b { 
  True -> False; 
  False -> True; 
};

isJust :: Maybe a -> Bool;
isJust m = match m { 
  Nothing -> False; 
  Just x -> True; 
};

-- Arithmetic and comparison work
add :: Int -> Int -> Int;
add x y = x + y;

lessThan :: Int -> Int -> Bool;
lessThan x y = x < y;

-- Constructor applications work
testBool :: Bool;
testBool = not True;

testMaybe :: Maybe Int;
testMaybe = Just 42;

testArithmetic :: Int;
testArithmetic = add 10 32;