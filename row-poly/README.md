# row-poly

Row polymorphism with scoped labels.

Implements a calculus of extensible records with scoped labels. Records may
contain duplicate labels, in which case the leftmost binding wins on selection.
The unification algorithm rewrites a row to bring a target label to the head,
with a side condition on the row tail that guarantees termination on the
classic non-terminating example.

## Surface syntax

```
e ::= x | n | true | false
    | \x -> e | e e | let x = e in e
    | { }                      empty record
    | { l = e, ... }           closed record
    | { l = e, ... | e }       record extension
    | e . l                    selection
    | { e - l }                restriction
    | { l := e | e }           update (sugar for { l = e | { r - l } })
```

## CLI

```bash
cargo run -p row-poly -- '\r -> r.x'
# \r -> r.x : {x : a | r} -> a

cargo run -p row-poly -- '{{x = 1, x = true} - x}.x'
# {{x = 1, x = true} - x}.x : Bool

cargo run -p row-poly -- repl
cargo run -p row-poly -- test tests/basic.fun
cargo run -p row-poly -- golden tests/basic.fun tests/basic.out
```

## Examples

| Input                       | Inferred type                    |
| --------------------------- | -------------------------------- |
| `\r -> r.x`                 | `{x : a \| r} -> a`              |
| `\r -> {z = 0 \| r}`        | `{r} -> {z : Int \| r}`          |
| `{x = 1, x = true}.x`       | `Int`                            |
| `{{x = 1, x = true} - x}.x` | `Bool`                           |
| `\r -> {x := 0 \| r}`       | `{x : a \| r} -> {x : Int \| r}` |
| `{x = 1}.y`                 | error: missing label `y`         |

## References

- _Extensible records with scoped labels._ TFP 2005.
  [PDF](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/scopedlabels.pdf)
