fib :: Int -> Int;
fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2);

main :: Unit;
main = printInt (fib 10);