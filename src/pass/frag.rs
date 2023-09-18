use graphql_parser::query::{
    Definition, Document, FragmentDefinition, InlineFragment, OperationDefinition, Selection,
};
use graphql_parser::schema::Text;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;

fn fix_selection<'a, T: Text<'a> + Clone + Debug>(
    sel: &mut Selection<'a, T>,
    frags: &HashMap<T, FragmentDefinition<'a, T>>,
) where
    T: Clone + Hash + PartialEq<T>,
{
    match sel {
        Selection::Field(f) => {
            for sel in &mut f.selection_set.items {
                fix_selection(sel, frags);
            }
        }
        Selection::FragmentSpread(s) => {
            let frag = frags
                .get(s.fragment_name.as_ref())
                .expect("Fragment not found");

            *sel = Selection::InlineFragment(InlineFragment {
                position: Default::default(),
                type_condition: Some(frag.type_condition.clone()),
                directives: frag.directives.clone(),
                selection_set: frag.selection_set.clone(),
            });
        }
        Selection::InlineFragment(f) => {
            for sel in &mut f.selection_set.items {
                fix_selection(sel, frags)
            }
        }
    }
}

pub fn inline_fragments<'a, T: Text<'a> + Clone + Debug>(
    doc: Document<'a, T>,
    op_name: Option<T>,
) -> OperationDefinition<'a, T>
where
    T: Clone + Hash + PartialEq<T>,
{
    let mut frags: HashMap<T, FragmentDefinition<'a, T>> = Default::default();
    let mut qs = vec![];
    let mut op = None;

    for def in doc.definitions {
        match def {
            Definition::Operation(o) => match o {
                OperationDefinition::SelectionSet(s) if op_name.is_none() => {
                    op = Some(OperationDefinition::SelectionSet(s));
                }
                OperationDefinition::SelectionSet(..) => {
                    panic!("Can't use shorthand with operationName specified");
                }
                OperationDefinition::Query(q) if q.name == op_name => {
                    op = Some(OperationDefinition::Query(q))
                }
                OperationDefinition::Mutation(m) if m.name == op_name => {
                    op = Some(OperationDefinition::Mutation(m));
                }
                OperationDefinition::Subscription(s) if s.name == op_name => {
                    op = Some(OperationDefinition::Subscription(s));
                }

                o => qs.push(o),
            },
            Definition::Fragment(f) => {
                frags.insert(f.name.clone(), f);
            }
        }
    }

    if op.is_none() && qs.len() == 1 {
        op = Some(qs.remove(0));
    }

    let Some(mut op) = op else {
        panic!("Did not resolve operation from query");
    };

    let names = frags.keys().cloned().collect::<Vec<_>>();
    for n in names {
        let mut taken = frags.remove(n.as_ref()).expect("Fragment not found");
        for sel in &mut taken.selection_set.items {
            fix_selection(sel, &frags);
        }
        frags.insert(n, taken);
    }

    match &mut op {
        OperationDefinition::SelectionSet(s) => {
            for sel in &mut s.items {
                fix_selection(sel, &frags)
            }
        }
        OperationDefinition::Query(q) => {
            for sel in &mut q.selection_set.items {
                fix_selection(sel, &frags)
            }
        }
        OperationDefinition::Mutation(m) => {
            for sel in &mut m.selection_set.items {
                fix_selection(sel, &frags)
            }
        }
        OperationDefinition::Subscription(s) => {
            for sel in &mut s.selection_set.items {
                fix_selection(sel, &frags)
            }
        }
    }

    op
}

#[test]
fn test_inline_pass() {
    let doc = r#"fragment Bar on Foo {
  bar {
    id
  }
}

fragment Baz on Foo {
  baz {
    id
  }
}


fragment MetaFoo on Foo {
  id
  ...Bar
  ...Baz
}

query Qux {
  foo {
    ...MetaFoo
  }
}
"#;
    let doc: Document<&str> = graphql_parser::parse_query(doc).unwrap();
    // let doc2 = inline_fragments(doc, Some("Qux"));
    // panic!("Resolved: {}", doc2.to_string());
}
