//! Integration tests for pattern matching implementation

use coc::ast::*;
use coc::context::Context;
use coc::typecheck::TypeChecker;

fn setup_nat_context() -> Context {
    let mut ctx = Context::new();

    // Add Nat type
    ctx.add_axiom("Nat".to_string(), Term::Sort(Universe::Const(1)));

    // Add constructors
    ctx.add_constructor("zero".to_string(), Term::Const("Nat".to_string()));
    ctx.add_constructor(
        "succ".to_string(),
        Term::Pi(
            "_".to_string(),
            Box::new(Term::Const("Nat".to_string())),
            Box::new(Term::Const("Nat".to_string())),
            false,
        ),
    );

    ctx
}

#[test]
fn test_simple_pattern_matching() {
    let mut type_checker = TypeChecker::new();
    let ctx = setup_nat_context();

    // Test: match zero with case zero => zero case succ(_) => zero
    let match_expr = Term::Match(
        Box::new(Term::Const("zero".to_string())),
        vec![
            MatchArm {
                pattern: Pattern::Constructor("zero".to_string(), vec![]),
                body: Term::Const("zero".to_string()),
            },
            MatchArm {
                pattern: Pattern::Constructor("succ".to_string(), vec![Pattern::Wildcard]),
                body: Term::Const("zero".to_string()),
            },
        ],
    );

    let result = type_checker.infer(&match_expr, &ctx);
    assert!(result.is_ok());

    // Should have type Nat
    match result.unwrap() {
        Term::Const(name) if name == "Nat" => {} // Success
        other => panic!("Expected Nat, got {:?}", other),
    }
}

#[test]
fn test_pattern_variable_binding() {
    let mut type_checker = TypeChecker::new();
    let ctx = setup_nat_context();

    // Test: match (succ zero) with case zero => zero case succ(x) => x
    let match_expr = Term::Match(
        Box::new(Term::App(
            Box::new(Term::Const("succ".to_string())),
            Box::new(Term::Const("zero".to_string())),
        )),
        vec![
            MatchArm {
                pattern: Pattern::Constructor("zero".to_string(), vec![]),
                body: Term::Const("zero".to_string()),
            },
            MatchArm {
                pattern: Pattern::Constructor(
                    "succ".to_string(),
                    vec![Pattern::Var("x".to_string())],
                ),
                body: Term::Var("x".to_string()), // Use the bound variable
            },
        ],
    );

    let result = type_checker.infer(&match_expr, &ctx);
    assert!(result.is_ok());
}

#[test]
fn test_pattern_matching_type_mismatch() {
    let mut type_checker = TypeChecker::new();
    let mut ctx = setup_nat_context();

    // Add Bool type and constructors
    ctx.add_axiom("Bool".to_string(), Term::Sort(Universe::Const(1)));
    ctx.add_constructor("true".to_string(), Term::Const("Bool".to_string()));
    ctx.add_constructor("false".to_string(), Term::Const("Bool".to_string()));

    // Test: match zero with case zero => true case succ(_) => zero
    // This should fail because branches have different types (Bool vs Nat)
    let match_expr = Term::Match(
        Box::new(Term::Const("zero".to_string())),
        vec![
            MatchArm {
                pattern: Pattern::Constructor("zero".to_string(), vec![]),
                body: Term::Const("true".to_string()), // Bool
            },
            MatchArm {
                pattern: Pattern::Constructor("succ".to_string(), vec![Pattern::Wildcard]),
                body: Term::Const("zero".to_string()), // Nat
            },
        ],
    );

    let result = type_checker.infer(&match_expr, &ctx);
    assert!(result.is_err()); // Should fail due to type mismatch
}
