-- Algebraic Data Types with constructors
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

fromMaybe :: a -> Maybe a -> a;
fromMaybe def m = match m {
  Nothing -> def;
  Just x -> x;
};

-- Polymorphic functions
id :: a -> a;
id x = x;

-- Higher-order functions with explicit parameters
map :: (a -> b) -> List a -> List b;
map f lst = match lst {
  Nil -> Nil;
  Cons x xs -> Cons (f x) (map f xs);
};

-- Arithmetic and comparison operations
add :: Int -> Int -> Int;
add x y = x + y;

multiply :: Int -> Int -> Int;
multiply x y = x * y;

lessThan :: Int -> Int -> Bool;
lessThan x y = x < y;

-- WORKING: Constructor applications
testBool :: Bool;
testBool = not True;

testMaybe :: Maybe Int;
testMaybe = Just 42;

testEither :: Either Bool Int;
testEither = Right 123;

testList :: List Int;
testList = Cons 1 (Cons 2 (Cons 3 Nil));

-- Function composition and application
composed :: Int;
composed = add (multiply 6 7) 8;

-- Nested pattern matching
listLength :: List a -> Int;
listLength lst = match lst {
  Nil -> 0;
  Cons x xs -> add 1 (listLength xs);
};
