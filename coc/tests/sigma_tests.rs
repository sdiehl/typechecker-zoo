//! Integration tests for Sigma type implementation

use coc::ast::*;
use coc::context::Context;
use coc::typecheck::TypeChecker;

#[test]
fn test_sigma_type_formation() {
    let mut type_checker = TypeChecker::new();
    let ctx = Context::new();

    // Test Σ (x : Type), Type
    let sigma_type = Term::Sigma(
        "x".to_string(),
        Box::new(Term::Sort(Universe::Const(1))), // Type
        Box::new(Term::Sort(Universe::Const(1))), // Type
    );

    let result = type_checker.infer(&sigma_type, &ctx);
    assert!(result.is_ok());

    // Should be Type 1 (or higher)
    match result.unwrap() {
        Term::Sort(_) => {} // Success
        other => panic!("Expected Sort, got {:?}", other),
    }
}

#[test]
fn test_pair_construction() {
    let mut type_checker = TypeChecker::new();
    let ctx = Context::new();

    // Create a pair (Type, Prop)
    let pair = Term::Pair(
        Box::new(Term::Sort(Universe::Const(1))), // Type
        Box::new(Term::Sort(Universe::Const(0))), // Prop
    );

    let result = type_checker.infer(&pair, &ctx);
    assert!(result.is_ok());

    // Should be a Sigma type
    match result.unwrap() {
        Term::Sigma(_, _, _) => {} // Success
        other => panic!("Expected Sigma type, got {:?}", other),
    }
}

#[test]
fn test_pair_projections() {
    let mut type_checker = TypeChecker::new();
    let ctx = Context::new();

    // Create a pair (Type, Prop)
    let pair = Term::Pair(
        Box::new(Term::Sort(Universe::Const(1))), // Type
        Box::new(Term::Sort(Universe::Const(0))), // Prop
    );

    // Test first projection
    let fst_proj = Term::Fst(Box::new(pair.clone()));
    let fst_result = type_checker.infer(&fst_proj, &ctx);
    assert!(fst_result.is_ok());

    // Test second projection
    let snd_proj = Term::Snd(Box::new(pair));
    let snd_result = type_checker.infer(&snd_proj, &ctx);
    assert!(snd_result.is_ok());
}
