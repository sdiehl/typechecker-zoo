# Constraint Solving

The constraint solver forms the beating heart of our Calculus of Constructions implementation, handling the complex task of resolving unknown terms, types, and universe levels through systematic constraint propagation and unification.

This is a non-trivial piece of software. That requirese quite a bit of care and thought. While this is a difficult piece of code, it is still far simpler than the 100k+ LOC implementations in Coq and Lean. This was the motivating example of this project to have a small CoC implementation that a sufficiently motivated (and probably caffeinated) undergraduate could read through in an afternoon.

So grab an espresso (or two or three) and buckle up!

## Core Data Structures

The solver operates on several fundamental data structures that capture the essence of constraint-based type inference in dependent type systems.

### Meta-Variable Management

```rust
#![struct!("coc/src/solver.rs", MetaInfo)]
```

Meta-variables represent unknown terms that must be resolved through constraint solving. Our implementation maintains comprehensive metadata for each meta-variable, enabling  dependency tracking and solution propagation:

```rust
#![struct!("coc/src/solver.rs", MetaId)]
```

The `MetaInfo` structure demonstrates how we track the complete lifecycle of unknown terms. The `context` field preserves the variable scope at the meta-variable's creation point, ensuring that solutions respect lexical scoping. The `dependencies` field tracks which other meta-variables must be resolved before this one can be solved, enabling topological ordering of constraint resolution.

### Constraint Representation

```rust
#![enum!("coc/src/solver.rs", Constraint)]
```

Our constraint system supports multiple categories of relationships that arise during dependent type checking:

**Unification Constraints** (`Unify`) represent the core requirement that two terms must be equal, with strength indicating solving priority. These constraints drive the primary unification algorithm and handle most structural equality requirements.

**Type Constraints** (`HasType`) ensure that terms inhabit their expected types, enabling type-directed constraint generation that guides the solver toward meaningful solutions.

**Universe Constraints** handle the complex relationships between universe levels, supporting both equality (`UnifyUniverse`) and ordering (`UniverseLevel`) requirements that maintain the hierarchy's consistency.

**Delayed Constraints** represent  patterns that cannot be solved immediately, waiting for specific meta-variables to be resolved before attempting solution.

### Constraint Strength and Prioritization

```rust
#![enum!("coc/src/solver.rs", ConstraintStrength)]
```

The constraint strength system enables intelligent solving order that maximizes the chances of finding solutions through a three-tier priority hierarchy. Required constraints represent fundamental relationships that must be satisfied for type checking to succeed and receive the highest priority during constraint resolution. These constraints typically arise from explicit type annotations or structural requirements that cannot be compromised.

Preferred constraints should be solved when possible but can be postponed if necessary to make progress on more critical constraints. These constraints often represent desirable properties or optimizations that improve the quality of solutions without being strictly necessary for correctness. Weak constraints provide guidance to the solver about preferred solutions but will not block progress if they prove unsolvable, allowing the algorithm to find workable solutions even when ideal solutions are not available.

## Constraint Solver

```rust
#![struct!("coc/src/solver.rs", Solver)]
```

The `Solver` represents the culmination of our constraint solving approach, integrating multiple  algorithms into a unified framework. The solver maintains several critical data structures:

**Meta-variable Registry** (`metas`) tracks all unknown terms with their complete metadata and dependency relationships.

**Constraint Management** (`constraints`, `queue`) maintains active constraints in a priority queue that enables intelligent solving order.

**Dependency Tracking** (`dependencies`) builds a graph of relationships between meta-variables and constraints, enabling topological solving and cycle detection.

**Advanced Features** (`enable_miller_patterns`, `enable_has_type_solving`) can be activated for handling higher-order unification patterns and complex type constraints.

### Meta-Variable Creation and Tracking

```rust
#![function!("coc/src/solver.rs", Solver::fresh_meta)]
```

Fresh meta-variable generation demonstrates how we maintain proper scoping and context information. Each meta-variable captures the variables that were in scope at its creation point, enabling proper variable capture analysis during solution.

```rust
#![function!("coc/src/solver.rs", Solver::fresh_universe_meta)]
```

Universe meta-variables represent unknown universe levels that get resolved through the universe constraint solver, enabling flexible universe polymorphism.

### Constraint Management and Dependencies

```rust
#![function!("coc/src/solver.rs", Solver::add_constraint)]
```

Adding constraints to the solver involves  dependency tracking that identifies which meta-variables appear in each constraint:

```rust
#![function!("coc/src/solver.rs", Solver::track_meta_occurrences)]
```

This dependency tracking enables several critical solver capabilities that ensure efficient and correct constraint resolution. The solver can wake up sleeping constraints when their dependent meta-variables get resolved, allowing previously blocked constraints to become active and potentially solvable. Topological ordering of constraint resolution ensures that constraints are solved in an order that respects their dependencies, maximizing the chances of successful resolution by handling simpler constraints before more complex ones that depend on their solutions.

The dependency tracking also enables detection of circular dependencies that might indicate unsolvable constraint systems. When the solver identifies cycles in the dependency graph, it can report these as errors rather than entering infinite loops, providing users with meaningful feedback about problematic constraint patterns.

```rust
#![function!("coc/src/solver.rs", Solver::collect_metas_in_constraint)]
```

The meta-variable collection algorithm recursively traverses constraint structures to identify all dependencies, building the complete dependency graph that guides solving order.

## Substitution System

```rust
#![struct!("coc/src/solver.rs", Substitution)]
```

Substitutions represent partial solutions to the constraint system, mapping meta-variables to their resolved terms. Our substitution system handles both term-level and universe-level substitutions with proper normalization.

### Substitution Application

```rust
#![function!("coc/src/solver.rs", Substitution::apply)]
```

The substitution application algorithm demonstrates the recursive nature of constraint solving in dependent type systems. When applying substitutions to terms like `Pi`, `Abs`, and `Let`, we must carefully handle variable binding and scope to avoid capture issues.

```rust
#![function!("coc/src/solver.rs", Substitution::apply_universe)]
```

Universe substitutions require special handling due to their arithmetic nature. The normalization process simplifies expressions like `Const(0) + 1` to `Const(1)`, maintaining canonical forms that improve unification success rates.

```rust
#![function!("coc/src/solver.rs", Substitution::normalize_universe_static)]
```

Static normalization performs universe-level arithmetic, combining constants and simplifying maximum expressions. This normalization is crucial for universe constraint solving, as it enables recognition of equivalent universe expressions that might otherwise appear different.

## Main Constraint Solving Algorithm

```rust
#![function!("coc/src/solver.rs", Solver::solve)]
```

The main solving loop demonstrates the iterative constraint propagation approach that drives our constraint solver through a systematic process of constraint resolution and solution propagation. The algorithm maintains several critical invariants that ensure both correctness and termination. The progress guarantee ensures that each iteration either successfully solves one or more constraints or detects unsolvable situations that can be reported as errors, preventing the solver from entering infinite loops without making meaningful progress.

Dependency respect ensures that constraints are solved in topological order based on meta-variable dependencies, so that simpler constraints whose solutions might enable the resolution of more complex constraints are prioritized appropriately. Solution propagation guarantees that when meta-variables are resolved, their solutions immediately propagate throughout the entire constraint system, potentially enabling the resolution of previously blocked constraints and maintaining consistency across the solver state.

### Constraint Selection Strategy

The solver uses intelligent constraint selection to maximize solving success:

```rust
let constraint_id = self.pick_constraint()?;
let constraint = self.constraints.remove(&constraint_id).unwrap();
```

The `pick_constraint` method prioritizes constraints based on several strategic factors that maximize solving efficiency. Constraint strength provides the primary ordering criterion, with required constraints taking precedence over preferred constraints, which in turn take precedence over weak constraints. This ensures that fundamental type checking requirements are addressed before optional optimizations or guidance hints.

The number of unknown meta-variables in each constraint provides a secondary ordering criterion, with constraints containing fewer unknowns receiving higher priority since they are more likely to be solvable immediately. Constraint type also influences prioritization, as unification constraints often resolve other constraints by instantiating meta-variables that appear in multiple constraint relationships.

### Solution Propagation

When a constraint is successfully solved, the solver propagates the solution throughout the system:

```rust
if progress {
    self.propagate_solution()?;
    let woken_constraints = self.wake_delayed_constraints()?;
    // Continue with newly awakened constraints...
}
```

Solution propagation involves a systematic process of updating the entire constraint system to reflect newly discovered solutions. Substitution application ensures that new solutions are applied to all remaining constraints in the system, potentially simplifying them or enabling their resolution. This step transforms the constraint system by replacing meta-variables with their concrete solutions wherever they appear.

Constraint wakeup activates delayed constraints that were waiting for the resolved meta-variables, bringing previously blocked constraints back into active consideration for solving. This mechanism ensures that the constraint resolution process can handle complex interdependencies where some constraints cannot be solved until others provide the necessary information.

Dependency updates modify the dependency graph to reflect newly resolved variables, removing solved meta-variables from dependency tracking and updating the topological ordering used for constraint selection. This maintenance ensures that the solver's internal data structures remain consistent and efficient as solutions accumulate.

## Unification in Dependent Type Systems

Unification in dependent type systems presents challenges beyond those encountered in simple type systems like System F. The interdependence between terms and types means that unifying types may require solving for unknown terms, while unifying terms may generate constraints on their types.

This interdependency creates several complications:

**Type-Term Dependencies**: When unifying `Π(x : A). B x` with `Π(y : A'). B' y`, we must unify both the parameter types `A` and `A'` and the dependent result types `B x` and `B' y`. The unification of result types depends on the solution to parameter type unification.

**Meta-Variable Scope Management**: Meta-variables representing unknown terms must respect the variable binding structure of dependent types. A meta-variable created in a particular binding context cannot be instantiated with a term that references variables outside that context.

**Higher-Order Meta-Variables**: In dependent type systems, meta-variables can represent unknown functions, leading to higher-order unification problems where we must solve for unknown function-level terms rather than just unknown ground terms.

## Unification Algorithm

The core unification algorithm handles the fundamental task of making two dependent types equal:

```rust
#![function!("coc/src/unification.rs", Unifier::unify)]
```

Our unification algorithm supports several  patterns:

**Structural Unification**: When both terms have the same head constructor, unification proceeds by recursively unifying subcomponents.

**Meta-Variable Instantiation**: When one side is a meta-variable, we create a substitution that maps the variable to the other term, subject to occurs checking.

**Higher-Order Patterns**: Advanced patterns like Miller patterns enable limited higher-order unification that remains decidable.

### Meta-Variable Resolution

```rust
match (&term1, &term2) {
    (Term::Meta(name), _) => {
        if let Some(meta_id) = self.parse_meta_name(name) {
            // Check if already solved
            if let Some(solution) = self.substitution.get(&meta_id) {
                return self.unify(&self.substitution.apply(solution), term2);
            }
            // Create new solution
            self.solve_meta_variable(meta_id, term2)
        }
    }
    // ... other cases
}
```

Meta-variable resolution involves a systematic multi-step process that ensures both correctness and consistency. Solution lookup first checks whether the meta-variable already has a solution from previous constraint resolution, avoiding redundant work and ensuring that existing solutions are properly utilized. The occurs check ensures that the proposed solution would not create infinite types by verifying that the meta-variable does not occur within its own solution term, preventing the creation of cyclic type definitions that would violate the soundness of the type system.

Context validation verifies that the solution respects variable scoping requirements, ensuring that the solution term does not reference variables that are not in scope at the meta-variable's binding site. This check is crucial for maintaining the lexical scoping discipline that dependent type systems require. Finally, solution recording adds the verified solution to the substitution system and propagates it throughout the constraint system, updating all constraints that reference the newly solved meta-variable.

### Dependent Type Unification

Unifying dependent types requires special handling of binding structures:

```rust
(Term::Pi(x1, ty1, body1, _), Term::Pi(x2, ty2, body2, _)) => {
    // Unify parameter types
    let param_subst = self.unify(ty1, ty2)?;

    // Unify bodies under extended context with alpha-renaming
    let renamed_body2 = self.alpha_rename(x2, x1, body2);
    let body_subst = self.unify_under_context(
        &param_subst.apply(body1),
        &param_subst.apply(&renamed_body2),
        x1
    )?;

    param_subst.compose(&body_subst)
}
```

This demonstrates the intricate complexity of dependent type unification, where multiple interdependent steps must be carefully orchestrated. Parameter types must unify first to establish the foundation for the dependent relationship, as the result type depends on the parameter type's structure and properties. Body types are then unified under the extended context that includes the parameter binding, ensuring that dependent references within the body are properly handled.

Alpha-renaming ensures that variable names do not interfere between the two dependent types being unified, preventing accidental capture or confusion between similarly named but distinct variables. The substitutions discovered during parameter unification must propagate to the body unification process, as changes to parameter types may affect the validity and structure of the dependent result types.

## Advanced Constraint Patterns

### Higher-Order Unification and Miller Patterns

Higher-order unification extends first-order unification to handle function-level unknowns, where meta-variables can represent unknown functions rather than just unknown terms. While first-order unification asks "what value makes these terms equal?", higher-order unification asks "what function makes these applications equal?"

The fundamental challenge of higher-order unification lies in its undecidability. Unlike first-order unification, which always terminates with either a solution or failure, general higher-order unification can run indefinitely without reaching a conclusion. This undecidability stems from the ability to construct arbitrarily complex function expressions that satisfy unification constraints.

Consider the higher-order unification problem `?F a b = g (h a) b`. The meta-variable `?F` represents an unknown function of two arguments. Potential solutions include `λx y. g (h x) y`, but also more complex forms like `λx y. g (h (id x)) y` where `id` is the identity function. The search space of possible solutions is infinite, making termination impossible to guarantee.

#### Miller Pattern Restrictions

Miller patterns resolve this undecidability by imposing syntactic restrictions on higher-order unification problems that restore decidability while preserving significant expressive power. A Miller pattern has the form `?M x₁ ... xₙ = t` where several critical conditions must be satisfied. All arguments `x₁ ... xₙ` must be distinct bound variables, ensuring that the pattern represents a proper functional relationship without duplication or confusion between parameters.

The meta-variable must be applied only to variables rather than complex terms, maintaining the "variable spine" property that prevents the exponential explosion of potential solutions that can arise when meta-variables are applied to arbitrary expressions. The term `t` must not contain the meta-variable `?M` itself, preventing occurs check violations that would lead to infinite types. Finally, abstraction safety requires that all free variables appearing in `t` must appear among the arguments `x₁ ... xₙ`, ensuring that the solution can be properly abstracted over the pattern variables.

These restrictions ensure that Miller pattern unification problems have unique most general unifiers when solutions exist, restoring decidability to this fragment of higher-order unification.

#### Miller Pattern Detection

Our implementation includes comprehensive Miller pattern detection:

```rust
#![function!("coc/src/solver.rs", Solver::is_simple_miller_pattern)]
```

The detection algorithm verifies that the left side forms a proper Miller pattern with distinct variable arguments and that the right side can be safely abstracted over those variables. This checking ensures that attempted solutions will satisfy the Miller pattern restrictions.

#### Miller Pattern Solving Algorithm

When Miller pattern solving is enabled, the solver handles these advanced patterns:

```rust
#![function!("coc/src/solver.rs", Solver::solve_miller_pattern)]
```

The Miller pattern solver constructs lambda abstractions that capture the relationship between the pattern variables and the target term. For patterns like `?M x = t`, the solution becomes `?M := λx. t`, provided that `t` satisfies the abstraction conditions.

Miller patterns represent a restricted form of higher-order unification that remains decidable while supporting many practical programming patterns that arise in dependent type theory and proof assistants.

### Delayed Constraint Resolution

Complex constraints that cannot be solved immediately get delayed until more information becomes available:

```rust
Constraint::Delayed { constraint, waiting_on, .. } => {
    if waiting_on.iter().all(|meta| self.is_solved(*meta)) {
        // All dependencies resolved - try solving now
        self.solve_constraint(*constraint)
    } else {
        // Still waiting - keep delayed
        Err(ConstraintError::NeedsMoreInfo)
    }
}
```

The delayed constraint system enables the solver to handle complex patterns that arise in  dependent type checking scenarios.

## Error Handling and Diagnostics

The constraint solver provides comprehensive error reporting that helps users understand solving failures:

```rust
pub enum ConstraintError {
    UnificationFailure { left: Term, right: Term, reason: String },
    OccursCheck { meta_var: MetaId, term: Term },
    ScopeViolation { meta_var: MetaId, escaped_vars: Vec<String> },
    CircularDependency { cycle: Vec<MetaId> },
    UniverseInconsistency { constraint: UniverseConstraint },
}
```

Each error type provides specific diagnostic information about why constraint solving failed, enabling users to understand and address the underlying issues. Unification failures present the conflicting terms along with a detailed explanation of why they cannot be made equal, helping programmers identify type mismatches and structural incompatibilities in their code. Occurs check violations identify situations where infinite types would result from a proposed solution, catching recursive type definitions that would violate the type system's soundness.

Scope violations detect variables that escape their intended lexical scope, typically occurring when meta-variable solutions reference variables that are not available in the solution's binding context. Circular dependencies identify unsolvable constraint cycles where constraints depend on each other in ways that prevent any progress, indicating fundamental problems in the constraint system structure. Universe inconsistencies indicate violations of the universe hierarchy, such as attempting to place a large universe inside a smaller one, which would compromise the type system's logical consistency.

And phew, that's it. We're done.
