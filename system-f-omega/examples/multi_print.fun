-- Test multiple print statements
seq :: Unit -> Unit -> Unit;
seq x y = y;

main :: Unit;
main = seq (printInt 10) (seq (printInt 20) (printInt 30));