# System F-ω

Polymorphic lambda calculus with higher-kinded types.

## Native Compilation

Compile `.fun` files to native executables:

```bash
# Compile to executable
cargo run -- compile fibonacci.fun -o fib

# Run the executable
./fib
# Output: 55
```

Example program (fibonacci.fun):
```haskell
fib :: Int -> Int;
fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2);

main :: Int;
main = fib 10;
```

## CLI Usage

```bash
# Type check a module
cargo run -- check module.fun

# Compile to native code
cargo run -- compile module.fun -o output

# Dump lexer tokens (debugging)
cargo run -- lex module.fun
```