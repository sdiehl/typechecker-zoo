data Bool = True | False;
data Maybe a = Nothing | Just a;
data List a = Nil | Cons a (List a);
data Pair a b = MkPair a b;

not :: Bool -> Bool;
not b = if b then False else True;

and :: Bool -> Bool -> Bool;
and x y = if x then y else False;

or :: Bool -> Bool -> Bool;
or x y = if x then True else y;

isNothing :: Maybe Int -> Bool;
isNothing m = match m {
    Nothing -> True;
    Just x -> False;
};

isJust :: Maybe Int -> Bool;
isJust m = match m {
    Nothing -> False;
    Just x -> True;
};

fromMaybe :: Int -> Maybe Int -> Int;
fromMaybe default m = match m {
    Nothing -> default;
    Just x -> x;
};

isEmpty :: List Int -> Bool;
isEmpty xs = match xs {
    Nil -> True;
    Cons y ys -> False;
};

add :: Int -> Int -> Int;
add x y = x + y;

subtract :: Int -> Int -> Int;
subtract x y = x - y;

multiply :: Int -> Int -> Int;
multiply x y = x * y;

divide :: Int -> Int -> Int;
divide x y = x / y;

isLessEqual :: Int -> Int -> Bool;
isLessEqual x y = x <= y;

testCondition :: Int -> Int -> Bool;
testCondition x y = if x <= y then not False else not True;

identity :: forall a. a -> a;
identity x = x;

const :: forall a b. a -> b -> a;  
const x y = x;

test_bool_true :: Bool;
test_bool_true = True;

test_bool_false :: Bool;
test_bool_false = False;

test_not :: Bool;
test_not = not True;

test_and :: Bool;
test_and = and True False;

test_or :: Bool;
test_or = or False True;

test_maybe_nothing :: Maybe Int;
test_maybe_nothing = Nothing;

test_is_nothing :: Bool;
test_is_nothing = isNothing Nothing;

test_is_just :: Bool;
test_is_just = isJust Nothing;

test_from_maybe :: Int;
test_from_maybe = fromMaybe 10 Nothing;

test_list_nil :: List Int;
test_list_nil = Nil;

test_is_empty :: Bool;
test_is_empty = isEmpty Nil;

test_arithmetic :: Int;
test_arithmetic = add 5 (multiply 3 4);

test_comparison :: Bool;
test_comparison = isLessEqual 10 15;

test_complex :: Int;
test_complex = if testCondition 5 10 then add 1 2 else subtract 10 3;

test_identity_int :: Int;
test_identity_int = identity 42;

test_identity_bool :: Bool;
test_identity_bool = identity True;

test_const :: Int;
test_const = const 100 200;

fst :: Pair Int Int -> Int;
fst p = match p {
    MkPair x y -> x;
};

snd :: Pair Int Int -> Int;
snd p = match p {
    MkPair x y -> y;
};

safeFib :: Int -> Maybe Int;
safeFib n = if n < 0 then Nothing else Just n;

fibBase :: Int -> Int;
fibBase n = if n <= 1 then n else add n (n - 1);

fibPair :: Int -> Pair Int Int;
fibPair n = if n <= 0 then MkPair 0 1 else MkPair 1 1;

test_fib_pair :: Pair Int Int;
test_fib_pair = MkPair 1 1;

test_fib_result :: Int;
test_fib_result = fibBase 5;

test_safe_fib :: Maybe Int;
test_safe_fib = safeFib 3;

test_fib_from_pair :: Int;
test_fib_from_pair = fst (fibPair 2);

test_maybe_fib :: Int;
test_maybe_fib = fromMaybe 0 (safeFib 4);

test_nested :: Bool;
test_nested = not (and (or True False) (isJust (safeFib 3)));