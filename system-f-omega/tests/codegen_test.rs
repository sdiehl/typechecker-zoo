#[cfg(feature = "codegen")]
use system_f_omega::codegen::erase;
#[cfg(feature = "codegen")]
use system_f_omega::core::{CoreTerm, CoreType};

#[cfg(feature = "codegen")]
#[test]
fn erase_identity() {
    let term = CoreTerm::Lambda {
        param: "x".to_string(),
        param_ty: CoreType::Con("Int".to_string()),
        body: Box::new(CoreTerm::Var("x".to_string())),
    };
    insta::assert_snapshot!(erase::erase(&term).pretty());
}

#[cfg(feature = "codegen")]
#[test]
fn erase_type_abstraction() {
    let term = CoreTerm::TypeLambda {
        param: "α".to_string(),
        body: Box::new(CoreTerm::Lambda {
            param: "x".to_string(),
            param_ty: CoreType::Var("α".to_string()),
            body: Box::new(CoreTerm::Var("x".to_string())),
        }),
    };
    insta::assert_snapshot!(erase::erase(&term).pretty());
}
