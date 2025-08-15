//! Integration tests for eta-conversion implementation

use coc::ast::*;
use coc::context::Context;
use coc::typecheck::TypeChecker;

#[test]
fn test_eta_conversion_basic() {
    // Test that λx. f x normalizes to f when x is not free in f
    let checker = TypeChecker::new();
    let ctx = Context::new();

    // Create term: λx. id x where id is a constant function
    // This should eta-convert to just 'id'
    let id_const = Term::Const("id".to_string());
    let x_var = Term::Var("x".to_string());
    let app_term = Term::App(Box::new(id_const.clone()), Box::new(x_var));
    let lambda_term = Term::Abs(
        "x".to_string(),
        Box::new(Term::Sort(Universe::Const(1))), /* Type annotation (doesn't matter for
                                                   * normalization) */
        Box::new(app_term),
    );

    // Normalize the lambda term
    let normalized = checker.normalize(&lambda_term, &ctx);

    // Should eta-convert to just 'id'
    assert_eq!(normalized, id_const);
}

#[test]
fn test_eta_conversion_with_free_variable() {
    // Test that λy. x y DOES normalize to x when y is not free in x
    let checker = TypeChecker::new();
    let ctx = Context::new();

    // Create term: λy. x y where x is a free variable
    // This SHOULD eta-convert to x because y does not occur free in x
    let x_var = Term::Var("x".to_string());
    let y_var = Term::Var("y".to_string());
    let app_term = Term::App(Box::new(x_var.clone()), Box::new(y_var));
    let lambda_term = Term::Abs(
        "y".to_string(),
        Box::new(Term::Sort(Universe::Const(1))),
        Box::new(app_term),
    );

    // Normalize the lambda term
    let normalized = checker.normalize(&lambda_term, &ctx);

    // Should eta-convert to just x
    assert_eq!(normalized, x_var);
}

#[test]
fn test_eta_conversion_no_conversion_when_var_occurs_free() {
    // Test that λx. (λy. x y) x does NOT eta-convert because x occurs free in (λy.
    // x y)
    let checker = TypeChecker::new();
    let ctx = Context::new();

    // Create term: λx. (λy. x y) x
    // Here, x occurs free in the function part (λy. x y), so NO eta-conversion
    // should happen
    let x_var = Term::Var("x".to_string());
    let y_var = Term::Var("y".to_string());
    let inner_app = Term::App(Box::new(x_var.clone()), Box::new(y_var));
    let inner_lambda = Term::Abs(
        "y".to_string(),
        Box::new(Term::Sort(Universe::Const(1))),
        Box::new(inner_app),
    );
    let outer_app = Term::App(Box::new(inner_lambda.clone()), Box::new(x_var.clone()));
    let outer_lambda = Term::Abs(
        "x".to_string(),
        Box::new(Term::Sort(Universe::Const(1))),
        Box::new(outer_app.clone()),
    );

    // Normalize the lambda term
    let normalized = checker.normalize(&outer_lambda, &ctx);

    // Should NOT eta-convert because x is free in the function part
    // But the inner lambda should eta-convert to x, so we expect: λx. x x
    let expected_inner_app = Term::App(Box::new(x_var.clone()), Box::new(x_var.clone()));
    let expected = Term::Abs(
        "x".to_string(),
        Box::new(Term::Sort(Universe::Const(1))),
        Box::new(expected_inner_app),
    );
    assert_eq!(normalized, expected);
}

#[test]
fn test_eta_conversion_complex_function() {
    // Test eta-conversion with a more complex constant function
    let checker = TypeChecker::new();
    let ctx = Context::new();

    // Create term: λx. (const 42) x where (const 42) doesn't contain x
    // This should eta-convert to just (const 42)
    let const_app = Term::App(
        Box::new(Term::Const("const".to_string())),
        Box::new(Term::Constructor("42".to_string(), vec![])),
    );
    let x_var = Term::Var("x".to_string());
    let app_term = Term::App(Box::new(const_app.clone()), Box::new(x_var));
    let lambda_term = Term::Abs(
        "x".to_string(),
        Box::new(Term::Sort(Universe::Const(1))),
        Box::new(app_term),
    );

    // Normalize the lambda term
    let normalized = checker.normalize(&lambda_term, &ctx);

    // Should eta-convert to just (const 42)
    assert_eq!(normalized, const_app);
}

#[test]
fn test_eta_conversion_nested_lambda() {
    // Test that eta-conversion works with nested lambdas: λx. λy. f x y
    // This should eta-convert to f through cascading conversions
    let checker = TypeChecker::new();
    let ctx = Context::new();

    // Create term: λx. λy. f x y
    // Inner lambda: λy. f x y should eta-convert to (f x)
    // Then outer lambda: λx. f x should eta-convert to f
    let f_const = Term::Const("f".to_string());
    let x_var = Term::Var("x".to_string());
    let y_var = Term::Var("y".to_string());

    let f_x = Term::App(Box::new(f_const.clone()), Box::new(x_var));
    let f_x_y = Term::App(Box::new(f_x), Box::new(y_var));

    let inner_lambda = Term::Abs(
        "y".to_string(),
        Box::new(Term::Sort(Universe::Const(1))),
        Box::new(f_x_y),
    );

    let outer_lambda = Term::Abs(
        "x".to_string(),
        Box::new(Term::Sort(Universe::Const(1))),
        Box::new(inner_lambda),
    );

    // Normalize the lambda term
    let normalized = checker.normalize(&outer_lambda, &ctx);

    // Should eta-convert completely to just f
    assert_eq!(normalized, f_const);
}
