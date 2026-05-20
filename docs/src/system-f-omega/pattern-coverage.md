# Pattern Coverage

Once the type checker has admitted a `match` expression as well-typed, a second pass decides whether the arms together cover every value the scrutinee could take, and whether any arm is shadowed by the earlier ones. These are the two halves of a single problem: a clause is reachable when there exists some value the previous matrix does not match but the new row does, and a match is exhaustive when no value escapes the matrix as a whole. The coverage pass implements the classical matrix decision procedure for both questions, as developed by the authors of _Warnings for pattern matching_, restricted to the algebraic datatypes that System Fω admits.

The pattern language is small. Wildcards and variables match any value of the relevant type, constructors match values of a specific shape with sub-patterns for each field.

\\[ \begin{align*}
\text{patterns} \quad p &::= \\_ \mid x \mid C\\, \overline{p}
\end{align*} \\]

The metavariable \\(p\\) ranges over patterns. The wildcard \\(\\_\\) matches any value without binding a name. The variable \\(x\\) also matches any value but binds the matched value to \\(x\\) in the arm body. The constructor pattern \\(C\\, \overline{p}\\) matches a value of the form \\(C\\, \overline{v}\\) when every sub-pattern \\(p_i\\) matches its corresponding \\(v_i\\), with the overline \\(\overline{p}\\) denoting the list of sub-patterns whose length is fixed by the arity of the constructor \\(C\\). The interesting case is the constructor: it carries the only structural information the algorithm can split on, and its sub-patterns recurse into the same grammar, which is what allows the procedure to discover non-exhaustiveness arbitrarily deep inside a value.

The pass operates on the core form of patterns, after surface variables have been desugared and after the type checker has resolved every constructor name to its declaring datatype.

```rust
#![enum!("system-f-omega/src/core.rs", CorePattern)]
```

## Constructor Environment

The decision procedure needs to know, for any constructor name encountered in a column, which other constructors share the same parent datatype. The environment is built once from the module's type declarations and indexes constructors by name.

```rust
#![struct!("system-f-omega/src/coverage.rs", CoverageEnv)]
```

The `ctors` map sends a constructor name to its parent type's name. The `siblings` map sends a parent name to the full list of its constructors together with their arities. When the algorithm meets the head constructor of a column it looks up the parent in `ctors`, asks `siblings` for the complete list, and compares the present heads against that list to decide whether the column is saturated. The arity of each sibling is read off the constructor's compiled type by peeling outer foralls and counting arrows.

```rust
#![function!("system-f-omega/src/coverage.rs", arrow_arity)]
```

## Matrix Operations

The state of the algorithm is a matrix of patterns, one row per surviving arm and one column per outstanding scrutinee position. Two operations rewrite the matrix into a simpler one as the algorithm descends.

The first is **specialization**. Given a head constructor \\(C\\) of arity \\(k\\), every row whose first cell matches \\(C\\) is rewritten so that the constructor pattern is replaced by its \\(k\\) sub-patterns, lengthening the row to width \\(k + n - 1\\). Rows headed by a wildcard or variable produce \\(k\\) fresh wildcards in place of the head, matching every possible argument tuple. Rows headed by a different constructor are dropped.

```rust
#![function!("system-f-omega/src/coverage.rs", specialize)]
```

The second is the **default matrix**. It keeps only the rows headed by a wildcard or variable, dropping the head cell, so the remaining matrix describes which values can flow past the first column without committing to any specific constructor.

```rust
#![function!("system-f-omega/src/coverage.rs", default_matrix)]
```

These two operations together form the recursive step. Specialization explores what happens when a value of a specific shape is matched, the default matrix explores what happens when the value does not match any of the constructors present in the column.

## Usefulness

The reachability check is phrased as the **usefulness** predicate. A new row is useful with respect to the matrix above it when there exists some value vector matched by the row but not by any earlier row. The recursion splits on the head pattern of the candidate row.

```rust
#![function!("system-f-omega/src/coverage.rs", is_useful)]
```

When the head is a constructor, the matrix is specialized at that constructor and the candidate row's sub-patterns are spliced into the front of its tail, then the question recurs at the reduced shape. When the head is a wildcard, the algorithm asks whether the column is saturated by the constructors present in the matrix. If every sibling of the parent datatype appears, the wildcard must match through one of them, so usefulness is established by finding any specific constructor at which the specialized matrix admits the wildcard row. If the column is not saturated, the wildcard can escape down a constructor that the matrix has never mentioned, so usefulness reduces to the question on the default matrix and the row's tail.

The driver runs this check on each arm as the matrix grows. If an arm is not useful, it is shadowed by some combination of earlier arms and an error is raised pointing at its index.

## Witness Search

Exhaustiveness is the dual question. The match is exhaustive when the entire matrix is useful against no row of all wildcards, which is the same as saying that the empty row at width one is not useful. Rather than answer that question directly, the algorithm constructs a **witness**: a value vector that no row matches. If a witness exists the match is non-exhaustive and the witness is reported as the missing pattern; if no witness exists the match is exhaustive.

```rust
#![enum!("system-f-omega/src/coverage.rs", Witness)]
```

Witness search mirrors the usefulness recursion but returns a value rather than a boolean.

```rust
#![function!("system-f-omega/src/coverage.rs", find_witness)]
```

When the current column has no constructors, the witness is a wildcard at this position and the search recurses on the default matrix. When some constructors appear, the algorithm first looks for a missing sibling: if any constructor of the parent datatype is absent from the column, a value built from that constructor with wildcard arguments is unmatched by every row, so the witness is built directly from the missing sibling. If every sibling is present, the witness must be built underneath one of the existing constructors, so the algorithm specializes the matrix at each present constructor in turn and asks whether a witness exists for the specialized problem. The first specialization that produces a witness gives the answer; the constructor's sub-patterns and the tail are reassembled into a full witness.

The key invariant is that the witness search descends into sub-patterns in the same way the type system descends into sub-types. A non-exhaustive match on `Maybe (Either Int Int)` that handles `Nothing` and `Just (Left _)` produces the witness `Just (Right _)` rather than just `Just _`, because the recursion on `Just`'s sub-pattern column rediscovers that `Right` is absent from the inner matrix and lifts a fresh `Right _` witness back through the outer `Just` constructor. The pretty-printed witness reads as a concrete missing pattern, which is what the error message reports.

```rust
#![trait_impl!("system-f-omega/src/coverage.rs", Display for Witness)]
```

## Driver

The driver assembles the two checks into a single pass over a list of arms. It threads the matrix forward, raises an `UnreachableMatchArm` error on the first arm that fails usefulness, and after every arm has been added asks for a witness against the full matrix. A witness becomes a `NonExhaustiveMatch` error carrying its pretty form.

```rust
#![function!("system-f-omega/src/coverage.rs", check_match)]
```

The module-level entry point walks every term definition and invokes `check_match` at every `Case` node it finds. The pass runs after bidirectional type checking has succeeded, so every constructor name in the patterns is known to belong to a declared datatype and every column has a parent in the environment built up front. The two error variants live in the standard error enum and flow through the same diagnostic pipeline as the type errors above them.

```rust
#![function!("system-f-omega/src/coverage.rs", check_module)]
```

A non-exhaustive match on a nested datatype illustrates the structural witness. The program

```text
data Either a b = Left a | Right b;
data Maybe a = Nothing | Just a;

test :: Maybe (Either Int Int) -> Int;
test m = match m {
    Nothing -> 0;
    Just (Left x) -> x;
};
```

is rejected with the diagnostic `Match is non-exhaustive; missing pattern: Just (Right _)`. The witness was produced by specialization at `Just`, which exposed the inner column over `Either`, where `Right` was found absent and lifted back out. The same procedure that flags this nested gap also rejects redundant arms: appending a second `Nothing -> 1` to the same body raises `Unreachable match arm at index 2`, since the new row's head specializes against an already-saturated default matrix and the recursion bottoms out with no useful position.
