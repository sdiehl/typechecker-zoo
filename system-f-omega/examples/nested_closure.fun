makeAdder :: Int -> Int -> Int;
makeAdder x y = x + y;

add10 :: Int -> Int;
add10 = makeAdder 10;

addTwice :: Int -> Int -> Int;
addTwice x y = add10 (add10 (x + y));

main :: Unit;
main = printInt (addTwice 5 7);