# Universe Polymorphism

Universe polymorphism enables definitions to abstract over universe levels, creating truly generic constructions that work across the entire universe hierarchy. Our implementation includes a dedicated universe constraint solver that handles the complex arithmetic and constraint relationships that arise in polymorphic universe contexts.

## Universe Constraint Solver

```rust
#![struct!("coc/src/universe_solver.rs", UniverseSolver)]
```

The `UniverseSolver` manages universe-level constraints independently from the main constraint solver, enabling specialized algorithms for universe arithmetic and level unification. This separation allows for efficient handling of universe polymorphism without complicating the main type checking algorithms.

### Universe Constraint Types

Universe constraints capture the relationships between universe levels that must be maintained for logical consistency:

```rust
#![enum!("coc/src/ast.rs", UniverseConstraint)]
```

The constraint system handles two fundamental relationships:

**Equality Constraints** (`Equal`) require two universe levels to be identical, arising from type equality requirements in dependent contexts where universe levels must match exactly.

**Ordering Constraints** (`LessEq`) ensure that one universe level is less than or equal to another, maintaining the predicativity requirements that prevent logical paradoxes.

### Universe Constraint Solving

```rust
#![function!("coc/src/universe_solver.rs", UniverseSolver::solve)]
```

The main solving algorithm processes constraint lists through iterative constraint resolution, ensuring that all universe relationships are satisfied consistently:

```rust
#![function!("coc/src/universe_solver.rs", UniverseSolver::solve_constraint)]
```

Individual constraint solving handles the different constraint types through specialized algorithms. Equality constraints use unification, while ordering constraints require more complex analysis to ensure the universe hierarchy remains consistent.

## Universe Unification Algorithm

```rust
#![function!("coc/src/universe_solver.rs", UniverseSolver::unify_universes)]
```

Universe unification demonstrates the complexity of working with universe-level arithmetic. The algorithm handles multiple universe expression forms:

1. **Variable Unification**: When unifying a universe variable with any expression, we create a substitution mapping after performing occurs checking to prevent infinite universe expressions.

2. **Constant Unification**: Universe constants can only unify with identical constants, ensuring that concrete levels like `Type 0` and `Type 1` remain distinct.

3. **Arithmetic Expressions**: Universe expressions like `u + 1` and `max(u, v)` require structural decomposition where we recursively unify the component universe expressions.

### Substitution and Normalization

```rust
#![function!("coc/src/universe_solver.rs", UniverseSolver::apply_substitution)]
```

Universe substitution application demonstrates the recursive nature of universe expressions. When substituting into compound expressions like `Add` or `Max`, we must recursively apply substitutions to all subcomponents while maintaining the arithmetic structure.

### Occurs Check for Universe Variables

```rust
#![function!("coc/src/universe_solver.rs", UniverseSolver::occurs_check)]
```

The occurs check prevents infinite universe expressions by ensuring that universe variables don't appear within their own solutions. This check must traverse the entire structure of universe expressions, including arithmetic operations and maximum expressions.

## Universe Expression Normalization

The universe solver includes normalization capabilities that simplify universe expressions to canonical forms:

```rust
match u {
    Universe::Add(base, n) => {
        let base_norm = self.normalize_universe_static(base);
        match base_norm {
            Universe::Const(m) => Universe::Const(m + n),
            _ => Universe::Add(Box::new(base_norm), *n),
        }
    }
    Universe::Max(u1, u2) => {
        let u1_norm = self.normalize_universe_static(u1);
        let u2_norm = self.normalize_universe_static(u2);
        match (&u1_norm, &u2_norm) {
            (Universe::Const(n1), Universe::Const(n2)) => Universe::Const((*n1).max(*n2)),
            _ => Universe::Max(Box::new(u1_norm), Box::new(u2_norm)),
        }
    }
    _ => u.clone(),
}
```

This normalization process:
- **Arithmetic Simplification**: Combines constants in addition expressions like `Const(2) + 3` becoming `Const(5)`
- **Maximum Computation**: Evaluates maximum expressions between constants
- **Canonical Forms**: Maintains normalized expressions that improve unification success

## Universe Polymorphic Definitions

Universe polymorphism enables definitions like:

```lean
def id.{u} (A : Sort u) (x : A) : A := x
```

The `.{u}` syntax introduces a universe parameter that can be instantiated at different levels:

```lean
-- id instantiated at Type 0
id_nat : Nat → Nat := id.{0} Nat

-- id instantiated at Type 1
id_type : Type → Type := id.{1} Type
```

### Fresh Variable Generation

```rust
#![function!("coc/src/universe_solver.rs", UniverseSolver::fresh_universe_var)]
```

Fresh universe variable generation ensures that each universe abstraction gets unique variable names, preventing conflicts in complex polymorphic definitions. The algorithm:

1. **Base Name Generation**: Starts with a descriptive base name
2. **Conflict Avoidance**: Checks against existing variables and substitutions
3. **Counter Extension**: Adds numeric suffixes when conflicts occur
4. **Uniqueness Guarantee**: Ensures the returned name is globally unique

### Substitution Management

```rust
#![function!("coc/src/universe_solver.rs", UniverseSolver::get_substitution)]
```

The solver provides access to resolved universe substitutions, enabling the main type checker to apply universe-level solutions throughout the type checking process.

```rust
#![function!("coc/src/universe_solver.rs", UniverseSolver::substitute_universe)]
```

Universe substitution application handles the complete resolution of universe expressions, recursively applying all accumulated substitutions to produce fully resolved universe levels.

## Integration with Type Checking

The universe solver integrates with the main type checking algorithm at several points:

**Type Formation**: When checking that types are well-formed, the universe solver ensures that universe level constraints are satisfied.

**Polymorphic Instantiation**: When instantiating polymorphic definitions, the universe solver generates fresh universe variables and maintains constraints between them.

**Definitional Equality**: When checking definitional equality between types with universe polymorphism, the universe solver ensures that universe relationships are preserved.

### Constraint Satisfaction Checking

```rust
#![function!("coc/src/universe_solver.rs", UniverseSolver::is_satisfiable)]
```

The satisfiability checker enables the type checker to verify that universe constraint sets have solutions before committing to particular type assignments. This early checking prevents backtracking in complex type inference scenarios.

## Universe Polymorphism Examples

Our implementation supports several forms of universe polymorphic definitions:

### Polymorphic Data Types

```lean
structure Pair.{u, v} (A : Sort u) (B : Sort v) : Sort (max u v) :=
  (fst : A)
  (snd : B)
```

The `Pair` type is polymorphic over two universe levels, with the result type living at the maximum of the argument universe levels.

### Polymorphic Functions

```lean
def const.{u, v} (A : Sort u) (B : Sort v) (x : A) (y : B) : A := x
```

The `const` function ignores its second argument and returns the first, working at any universe levels for both argument types.

### Universe Level Arithmetic

```lean
def lift.{u} (A : Sort u) : Sort (u + 1) := A
```

The `lift` operation moves a type from universe `u` to universe `u + 1`, demonstrating universe level arithmetic in type expressions.

## Failure modes

There are a couple of failure cases we handle with custom errors:

```rust
#![function!("coc/src/universe_solver.rs", UniverseSolver::universe_to_string)]
```

The failures are categorized into three main types:

- **Unification Failures**: Show the specific universe levels that couldn't be unified
- **Occurs Check Violations**: Identify infinite universe expressions
- **Arithmetic Inconsistencies**: Point out invalid universe level arithmetic
