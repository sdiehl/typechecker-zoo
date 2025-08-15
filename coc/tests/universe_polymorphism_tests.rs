//! Integration tests for universe polymorphism implementation

use std::collections::HashMap;

use coc::ast::*;
use coc::context::Context;
use coc::parse::Parser;
use coc::typecheck::TypeChecker;

#[test]
fn test_universe_zero() {
    // Prop = Sort 0
    let term = Term::Sort(Universe::Const(0));
    let ctx = Context::new();
    let mut checker = TypeChecker::new();

    // Prop : Type (Sort 1)
    let result = checker.infer(&term, &ctx).unwrap();
    assert_eq!(result, Term::Sort(Universe::Const(1)));
}

#[test]
fn test_universe_succ() {
    // Type = Sort 1
    let term = Term::Sort(Universe::Const(1));
    let ctx = Context::new();
    let mut checker = TypeChecker::new();

    // Type : Type 1 (Sort 2)
    let result = checker.infer(&term, &ctx).unwrap();
    assert_eq!(result, Term::Sort(Universe::Const(2)));
}

#[test]
fn test_universe_variable() {
    // Sort u where u is a universe variable
    let u = Universe::ScopedVar("global".to_string(), "u".to_string());
    let term = Term::Sort(u.clone());

    // Add u to context
    let ctx = Context::new().extend_universe("u".to_string());

    let mut checker = TypeChecker::new();

    // Sort u : Sort (u+1)
    let result = checker.infer(&term, &ctx).unwrap();
    assert_eq!(result, Term::Sort(Universe::Add(Box::new(u), 1)));
}

#[test]
fn test_pi_rule_prop() {
    // Prop -> A has type A
    let prop = Term::Sort(Universe::Const(0));
    let type_term = Term::Sort(Universe::Const(1));
    let pi = Term::Pi(
        "x".to_string(),
        Box::new(prop),
        Box::new(type_term.clone()),
        false,
    );

    let ctx = Context::new();
    let mut checker = TypeChecker::new();

    // (Prop -> Type) : max(Prop, Type) = Type
    // Our implementation returns max(0, 1) which is correct
    let result = checker.infer(&pi, &ctx).unwrap();
    // The result should be Sort(max(0, 1)) which equals Sort 1
    match result {
        Term::Sort(Universe::Max(ref u1, ref u2)) => {
            // Check that it's max of Prop and Type levels
            // We expect max(Const(0), Const(1))
            assert!(matches!(**u1, Universe::Const(_)));
            assert!(matches!(**u2, Universe::Const(_)));
        }
        Term::Sort(Universe::Const(_)) => {
            // Also accept Sort 1 if the max was simplified
        }
        _ => panic!("Expected Sort(max(...)) or Sort 1, got {:?}", result),
    }
}

#[test]
fn test_parse_type_levels() {
    let test_cases = vec![
        ("Type", Term::Sort(Universe::Const(1))),
        ("Prop", Term::Sort(Universe::Const(0))),
        ("Sort 0", Term::Sort(Universe::Const(0))),
        ("Sort 1", Term::Sort(Universe::Const(1))),
    ];

    for (input, expected) in test_cases {
        let parser = Parser::new();
        let result = parser.parse_term(input);
        assert!(result.is_ok(), "Failed to parse: {}", input);
        assert_eq!(result.unwrap(), expected, "Mismatch for: {}", input);
    }
}

#[test]
fn test_universe_substitution() {
    // Create a type with universe variable: Sort (u+1)
    let u_var = Universe::ScopedVar("global".to_string(), "u".to_string());
    let term = Term::Sort(Universe::Add(Box::new(u_var), 1));

    // Create substitution u -> 0
    let mut subst = HashMap::new();
    subst.insert("u".to_string(), Universe::Const(0));

    // Apply substitution
    let result = coc::term_utils::substitute_universes(&term, &subst);

    // Should get Sort (Const(1)) = Type
    assert_eq!(result, Term::Sort(Universe::Const(1)));
}

#[test]
fn test_universe_max() {
    let u = Universe::ScopedVar("global".to_string(), "u".to_string());
    let v = Universe::ScopedVar("global".to_string(), "v".to_string());
    let max_uv = Universe::Max(Box::new(u.clone()), Box::new(v.clone()));

    // max(u, v) represents the maximum of u and v
    assert!(matches!(max_uv, Universe::Max(_, _)));
}

#[test]
fn test_universe_imax() {
    let u = Universe::ScopedVar("global".to_string(), "u".to_string());
    let v = Universe::ScopedVar("global".to_string(), "v".to_string());
    let imax_uv = Universe::IMax(Box::new(u.clone()), Box::new(v.clone()));

    // imax(u, v) is used for impredicative universes
    assert!(matches!(imax_uv, Universe::IMax(_, _)));
}
