-- Test data types with constructors

data Bool = True | False;

data Maybe a = Nothing | Just a;

data Either a b = Left a | Right b;

-- Functions using constructors
not :: Bool -> Bool;
not b = match b { True -> False; False -> True; };

isJust :: Maybe a -> Bool;
isJust m = match m { Nothing -> False; Just x -> True; };

fromMaybe :: a -> Maybe a -> a;
fromMaybe def m = match m { Nothing -> def; Just x -> x; };

-- Test expressions
testBool :: Bool;
testBool = not True;

testMaybe :: Maybe Bool;
testMaybe = Just True;

testEither :: Either Bool Bool;
testEither = Left True;