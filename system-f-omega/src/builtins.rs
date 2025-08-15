use std::collections::HashMap;

use crate::core::{CoreType, DataConstructor, Kind};

/// Add built-in types to the compilation environment
///
/// - Int: A simple concrete type with kind *
/// - Bool: A concrete type with constructors True and False
/// - List: A parametric type constructor with kind * -> * and constructors Nil
///   and Cons
pub fn add_builtin_types(
    type_constructors: &mut HashMap<String, (Kind, Vec<DataConstructor>)>,
    data_constructors: &mut HashMap<String, CoreType>,
) {
    // Int type
    type_constructors.insert("Int".to_string(), (Kind::Star, vec![]));

    // Bool type
    type_constructors.insert(
        "Bool".to_string(),
        (
            Kind::Star,
            vec![
                DataConstructor {
                    name: "True".to_string(),
                    ty: CoreType::Con("Bool".to_string()),
                },
                DataConstructor {
                    name: "False".to_string(),
                    ty: CoreType::Con("Bool".to_string()),
                },
            ],
        ),
    );

    // List type constructor
    let list_kind = Kind::Arrow(Box::new(Kind::Star), Box::new(Kind::Star));
    type_constructors.insert("List".to_string(), (list_kind, vec![]));

    // Add List constructors (parametric)
    // Nil :: forall a. List a
    let nil_type = CoreType::Forall(
        "a".to_string(),
        Box::new(CoreType::App(
            Box::new(CoreType::Con("List".to_string())),
            Box::new(CoreType::Var("a".to_string())),
        )),
    );

    // Cons :: forall a. a -> List a -> List a
    let cons_type = CoreType::Forall(
        "a".to_string(),
        Box::new(CoreType::Arrow(
            Box::new(CoreType::Var("a".to_string())),
            Box::new(CoreType::Arrow(
                Box::new(CoreType::App(
                    Box::new(CoreType::Con("List".to_string())),
                    Box::new(CoreType::Var("a".to_string())),
                )),
                Box::new(CoreType::App(
                    Box::new(CoreType::Con("List".to_string())),
                    Box::new(CoreType::Var("a".to_string())),
                )),
            )),
        )),
    );

    data_constructors.insert("Nil".to_string(), nil_type);
    data_constructors.insert("Cons".to_string(), cons_type);

    // Add built-in Bool constructors
    data_constructors.insert("True".to_string(), CoreType::Con("Bool".to_string()));
    data_constructors.insert("False".to_string(), CoreType::Con("Bool".to_string()));
}
