add :: Int -> Int -> Int;
add x y = x + y;

add5 :: Int -> Int;
add5 = add 5;

main :: Unit;
main = printInt (add5 10);