//! Basic tests for the code generation pipeline

use system_f_omega::codegen::erase;
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
