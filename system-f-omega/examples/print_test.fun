add :: Int -> Int -> Int;
add x y = x + y;

test1 :: Unit;
test1 = printInt 42;

test2 :: Unit;
test2 = printInt (add 10 32);

main :: Unit;
main = test2;