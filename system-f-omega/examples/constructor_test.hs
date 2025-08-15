-- Test direct constructor usage

data Maybe a = Nothing | Just a;

-- Test direct constructor application
makeJust :: a -> Maybe a;
makeJust x = Just x;

-- Test constructor without arguments
makeNothing :: Maybe a;
makeNothing = Nothing;

testConstructor :: Maybe Bool;
testConstructor = makeJust True;