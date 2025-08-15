-- Comprehensive test of working features

-- Basic ADTs
data Bool = True | False;
data Maybe a = Nothing | Just a;
data Either a b = Left a | Right b;
data List a = Nil | Cons a (List a);

-- Functions with pattern matching
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

-- Polymorphic function
id :: a -> a;
id x = x;

-- Higher-order function
map :: (a -> b) -> List a -> List b;
map f lst = match lst {
  Nil -> Nil;
  Cons x xs -> Cons (f x) (map f xs);
};

-- Arithmetic operations
add :: Int -> Int -> Int;
add x y = x + y;

-- Comparison operations  
lessThan :: Int -> Int -> Bool;
lessThan x y = x < y;

-- Lambda expressions (using function definition syntax)
double :: Int -> Int;
double x = x * 2;

-- Constructor applications
testBool :: Bool;
testBool = not True;

testMaybe :: Maybe Int;
testMaybe = Just 42;

testEither :: Either Bool Int;
testEither = Right 123;

testList :: List Int;
testList = Cons 1 (Cons 2 Nil);

-- Function application
testMap :: List Int;
testMap = map double testList;