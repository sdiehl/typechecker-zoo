//! Basic tests for the code generation pipeline

use system_f_omega::codegen::{closure, erase};
use system_f_omega::core::{CoreTerm, CoreType};

#[test]
fn test_erase_identity() {
    // λx:Int. x
    let term = CoreTerm::Lambda {
        param: "x".to_string(),
        param_ty: CoreType::Con("Int".to_string()),
        body: Box::new(CoreTerm::Var("x".to_string())),
    };

    let erased = erase::erase(&term);
    assert_eq!(erased.pretty(), "λx. x");
}

#[test]
fn test_erase_type_abstraction() {
    // Λα. λx:α. x
    let term = CoreTerm::TypeLambda {
        param: "α".to_string(),
        body: Box::new(CoreTerm::Lambda {
            param: "x".to_string(),
            param_ty: CoreType::Var("α".to_string()),
            body: Box::new(CoreTerm::Var("x".to_string())),
        }),
    };

    let erased = erase::erase(&term);
    assert_eq!(erased.pretty(), "λx. x");
}

#[test]
fn test_closure_convert_constant() {
    // 42
    let term = CoreTerm::LitInt(42);
    let erased = erase::erase(&term);
    let program = closure::closure_convert(&erased);

    assert_eq!(program.functions.len(), 0);
    assert_eq!(program.main.pretty(), "42");
}

#[test]
fn test_closure_convert_identity() {
    // λx. x
    let term = CoreTerm::Lambda {
        param: "x".to_string(),
        param_ty: CoreType::Con("Int".to_string()),
        body: Box::new(CoreTerm::Var("x".to_string())),
    };

    let erased = erase::erase(&term);
    let program = closure::closure_convert(&erased);

    assert_eq!(program.functions.len(), 1);
    assert_eq!(program.functions[0].param, "x".to_string());
    assert_eq!(program.functions[0].free_vars.len(), 0);
}

#[test]
fn test_closure_convert_capture() {
    // λx. λy. x
    let term = CoreTerm::Lambda {
        param: "x".to_string(),
        param_ty: CoreType::Con("Int".to_string()),
        body: Box::new(CoreTerm::Lambda {
            param: "y".to_string(),
            param_ty: CoreType::Con("Int".to_string()),
            body: Box::new(CoreTerm::Var("x".to_string())),
        }),
    };

    let erased = erase::erase(&term);
    let program = closure::closure_convert(&erased);

    // Should have two functions
    assert_eq!(program.functions.len(), 2);

    // Inner function should capture x
    let inner_func = &program.functions[0];
    assert_eq!(inner_func.param, "y".to_string());
    assert!(inner_func.free_vars.contains(&"x".to_string()));
}

#[test]
fn test_value_tagging() {
    use system_f_omega::codegen::value::{Tag, Value};

    let int_val = Value::int(42);
    assert_eq!(int_val.tag(), Tag::Integer);
    assert_eq!(int_val.as_int(), 42);

    let bool_val = Value::bool(true);
    assert_eq!(bool_val.tag(), Tag::Boolean);
    assert_eq!(bool_val.as_bool(), true);
}
