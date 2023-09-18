use crate::Variables;
use graphql_parser::query::{
    Definition, Document, FragmentDefinition, InlineFragment, OperationDefinition, Selection,
    SelectionSet, VariableDefinition,
};
use graphql_parser::schema::Text;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;

fn resolve_vars<'a, T: Text<'a>>(
    set: &mut SelectionSet<'a, T>,
    defs: &[VariableDefinition<'a, T>],
    vars: &Variables,
) {
    // for sel in &mut set.items {
    //     match sel {
    //         Selection::Field(f) => {
    //             f.
    //         }
    //         Selection::FragmentSpread(_) => {}
    //         Selection::InlineFragment(_) => {}
    //     }
    // }
}
