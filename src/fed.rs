use graphql_parser::query::{Field, InlineFragment, Query, Selection, SelectionSet, TypeCondition, VariableDefinition};
use graphql_parser::schema::{EnumType, EnumValue, InputObjectType, InterfaceType, ScalarType, Text, UnionType};
use graphql_parser::schema::{
    Definition, DirectiveDefinition, Document, InputValue, ObjectType, SchemaDefinition, Type,
    TypeDefinition, Value,
};
use graphway_macros::FromDirective;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::Deref;
use graphql_parser::parse_query;
use graphql_parser::tokenizer::TokenStream;
use indexmap::IndexMap;
use crate::SGraph;

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct Enum<'a, T: Text<'a>>(T, PhantomData<&'a ()>);

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct Str<'a, T: Text<'a>>(pub T, PhantomData<&'a ()>);

pub trait FromValue<'a, T: Text<'a>> {
    fn from_value(v: &Value<'a, T>) -> Self;
}

impl<'a, T: Text<'a>> FromValue<'a, T> for Enum<'a, T> {
    fn from_value(v: &Value<'a, T>) -> Self {
        Self(v.as_enum().unwrap().clone(), PhantomData)
    }
}

impl<'a, T: Text<'a>> FromValue<'a, T> for Str<'a, T> {
    fn from_value(v: &Value<'a, T>) -> Self {
        Str(v.as_string().expect("Expected string").clone(), PhantomData)
    }
}

impl<'a, T: Text<'a>> FromValue<'a, T> for bool {
    fn from_value(v: &Value<'a, T>) -> Self {
        match v {
            Value::Boolean(c) => c.clone(),
            _ => panic!("Expected boolean"),
        }
    }
}

impl<'a, T: Text<'a>> FromValue<'a, T> for usize {
    fn from_value(v: &Value<'a, T>) -> Self {
        match v {
            Value::Int(n) => n
                .as_i64()
                .expect("not i64")
                .try_into()
                .expect("Not convertible to usize"),
            n => panic!("Expected number, got {n:?}"),
        }
    }
}

#[derive(Debug, Clone, FromDirective)]
pub struct JoinField<'a, T: Text<'a>> {
    pub graph: Enum<'a, T>,
    pub requires: Option<Str<'a, T>>,
    pub provides: Option<Str<'a, T>>,
    #[name("type")]
    pub r#type: Option<Str<'a, T>>,
    pub external: Option<bool>,
    #[name("override")]
    pub r#override: Option<Str<'a, T>>,
}

#[derive(Debug, Clone, FromDirective)]
pub struct JoinGraph<'a, T: Text<'a>> {
    pub name: Str<'a, T>,
    pub url: Str<'a, T>,
    #[dirignore]
    _p: PhantomData<&'a T>,
}

#[derive(Debug, Clone, FromDirective)]
pub struct JoinType<'a, T: Text<'a>> {
    graph: Enum<'a, T>,
    key: Option<Str<'a, T>>,
}

#[derive(Debug, Clone, FromDirective)]
pub struct JoinEnum<'a, T: Text<'a>> {
    graph: Enum<'a, T>,
}

#[derive(Debug, Clone, FromDirective)]
pub struct JoinUnionMember<'a, T: Text<'a>> {
    graph: Enum<'a, T>,
    member: Str<'a, T>,
}

#[derive(Debug, Clone, FromDirective)]
pub struct Link<'a, T: Text<'a>> {
    url: Option<Str<'a, T>>,
    #[name("as")]
    r#as: Option<Str<'a, T>>,
    #[name("for")]
    r#for: Option<Enum<'a, T>>,
}

#[derive(Debug, Clone)]
pub struct FieldT<'a, T: Text<'a>> {
    pub join_field: Vec<JoinField<'a, T>>,

    pub arguments: HashMap<T, InputValue<'a, T>>,
    pub field_type: Type<'a, T>,
}

impl<'a, T: Text<'a>> From<graphql_parser::schema::Field<'a, T>> for FieldT<'a, T> {
    fn from(value: graphql_parser::schema::Field<'a, T>) -> Self {
        let join_field = value
            .directives
            .iter()
            .filter(|d| d.name.as_ref() == "join__field")
            .map(JoinField::from)
            .collect();

        FieldT {
            join_field,
            arguments: value.arguments.into_iter().map(|i| (i.name.clone(), i)).collect(),
            field_type: value.field_type,
        }
    }
}


pub type Fields<'a, T> = HashMap<T, FieldT<'a, T>>;

#[derive(Debug, Clone)]
pub struct ObjectT<'a, T: Text<'a>> {
    pub name: T,
    pub join_type: Vec<JoinType<'a, T>>,

    pub implements_interfaces: Vec<T>,
    pub fields: Fields<'a, T>,
}

impl<'a, T: Text<'a>> From<ObjectType<'a, T>> for ObjectT<'a, T> {
    fn from(value: ObjectType<'a, T>) -> Self {
        let join_type = value
            .directives
            .iter()
            .filter(|d| d.name.as_ref() == "join__type")
            .map(JoinType::from)
            .collect();

        Self {
            name: value.name,
            join_type,
            implements_interfaces: value.implements_interfaces,
            fields: value
                .fields
                .into_iter()
                .map(|v| (v.name.clone(), FieldT::from(v)))
                .collect(),
        }
    }
}

impl<'a, T: Text<'a>> From<InterfaceType<'a, T>> for ObjectT<'a, T> {
    fn from(value: InterfaceType<'a, T>) -> Self {
        let join_type = value
            .directives
            .iter()
            .filter(|d| d.name.as_ref() == "join__type")
            .map(JoinType::from)
            .collect();


        Self {
            name: value.name,
            join_type,
            implements_interfaces: value.implements_interfaces,
            fields: value
                .fields
                .into_iter()
                .map(|v| (v.name.clone(), FieldT::from(v)))
                .collect(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct UnionT<'a, T: Text<'a>> {
    pub join_type: Vec<JoinType<'a, T>>,

    pub types: Vec<T>,
}

impl<'a, T: Text<'a>> From<UnionType<'a, T>> for UnionT<'a, T> {
    fn from(value: UnionType<'a, T>) -> Self {
        let join_type = value
            .directives
            .iter()
            .filter(|d| d.name.as_ref() == "join__type")
            .map(JoinType::from)
            .collect();

        Self {
            join_type,
            types: value.types,
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeT<'a, T: Text<'a>> {
    Object(ObjectT<'a, T>),
    Union(UnionT<'a, T>),
    Enum(EnumT<'a, T>),
    Scalar(ScalarType<'a, T>),
}


#[derive(Debug, Clone)]
pub struct EnumV<'a, T: Text<'a>> {
    pub join_enum: Vec<JoinEnum<'a, T>>,
    pub join_graph: Option<JoinGraph<'a, T>>,
    pub value: T,
}

impl<'a, T: Text<'a>> From<&EnumValue<'a, T>> for EnumV<'a, T> {
    fn from(value: &EnumValue<'a, T>) -> Self {
        let join_enum = value.directives.iter()
            .filter(|it| it.name.as_ref() == "join__enum")
            .map(JoinEnum::from)
            .collect();

        let join_graph = value.directives.iter()
            .filter(|it| it.name.as_ref() == "join__graph")
            .map(JoinGraph::from)
            .next();

        Self {
            join_enum,
            join_graph,
            value: value.name.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct EnumT<'a, T: Text<'a>> {
    pub join_type: Vec<JoinType<'a, T>>,
    pub values: Vec<EnumV<'a, T>>,
}

impl<'a, T: Text<'a>> From<&EnumType<'a, T>> for EnumT<'a, T> {
    fn from(value: &EnumType<'a, T>) -> Self {
        let join_type = value.directives.iter()
            .filter(|it| it.name.as_ref() == "join__type")
            .map(JoinType::from)
            .collect();
        Self {
            join_type,
            values: value.values.iter().map(EnumV::from).collect(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Graph<'a, T: Text<'a>> {
    pub schema: SchemaDefinition<'a, T>,
    // pub directives: HashMap<T, DirectiveDefinition<'a, T>>,
    // pub types: HashMap<T, TypeDefinition<'a, T>>,

    pub types: HashMap<T, TypeT<'a, T>>,

    pub objects: HashMap<T, ObjectT<'a, T>>,
    pub enums: HashMap<T, EnumT<'a, T>>,
    pub scalars: HashMap<T, ScalarType<'a, T>>,

    pub query: Option<ObjectT<'a, T>>,
    pub subscription: Option<ObjectT<'a, T>>,
    pub mutation: Option<ObjectT<'a, T>>,

    pub subgraphs: HashMap<T, SubGraph<'a, T>>,
}

#[derive(Debug, Clone)]
pub struct SubGraph<'a, T: Text<'a>> {
    pub key: T,
    pub name: Str<'a, T>,
    pub url: Str<'a, T>,
    _p: PhantomData<&'a ()>,
}

pub struct Fetch<'a, T: Text<'a>> {
    service: Str<'a, T>,
    query: Query<'a, T>,
}

pub enum Plan<'a, T: Text<'a>> {
    // Simple fetch
    Fetch {
        service: Str<'a, T>,
        query: Query<'a, T>,
    },
    // Fetch that resolves entity fields from previous output,
    // Creates a subgraph fetch for unresolved entity fields, and
    // merges the output to the state
    Flatten {
        // TODO: https://www.apollographql.com/docs/federation/query-plans/
        path: String,
        // What fields do we need to resolve from previous state
        repr: Vec<Selection<'a, T>>,
        // What service are we reaching
        serv: String,
        // What fields are we fetching
        fetch: Vec<Selection<'a, T>>,
    },
    // Runs provided steps concurrently, merging the results
    Sequence(Vec<Plan<'a, T>>),
    // Runs provided steps in parallel, merging the results
    Parallel(Vec<Plan<'a, T>>),
}

#[derive(Debug)]
enum Seg<T> {
    Array,
    Item(T),
}

pub struct Planner<'a, T: Text<'a>> {
    graph: &'static Graph<'static, &'static str>,
    subgraph: Option<&'a str>,
    path: Vec<Seg<T>>,

    pub select: Vec<SelectionSet<'a, T>>,
    _p: PhantomData<&'a T>,
}

impl<'a, T: Text<'a>> Planner<'a, T> {
    pub fn new(graph: SGraph, obj: &'a ObjectT<'static, &'static str>) -> Self {
        Self {
            graph,
            subgraph: None,
            path: vec![],
            select: vec![],
            _p: Default::default(),
        }
    }
    pub fn field(&mut self, obj: &ObjectT<'static, &'static str>, qf: graphql_parser::query::Field<'a, T>) {
        println!("Resolving: {:?} in {:?}\n\n", qf.name, obj);

        let ft = obj.fields.get(qf.name.as_ref())
            .expect("Did not resolve field");

        let restore_sg = match self.subgraph {
            Some(current) => {
                if ft.join_field.is_empty() {
                    // Can resolve from this subgraph
                } else if ft.join_field.iter().any(|it| it.graph.0 == current) {
                    // can resolve from this subgraph
                } else {
                    for jt in obj.join_type.iter().filter(|it| it.key != None) {
                        for jf in &ft.join_field {
                            if jt.graph.0 == jf.graph.0 {
                                // panic!("Will need to resolve entity at path {:?} from {:?} using: {:?}", self.path, jt.graph.0, jt.key.clone().unwrap());
                                self.subgraph = Some(jt.graph.0);
                                let key = jt.key.as_ref().unwrap();
                                let mut s = TokenStream::<'a>::new(key.0);
                                let (ss, _) = graphql_parser::query::raw_selection_set::<T>(&mut s)
                                    .expect("Cant parse selection set from str");
                                self.fields()
                            }
                        }
                    }
                    // panic!("Did not find any way to resovle field")
                    // if let Some(switch) = obj.join_type.iter().filter(|i| i.graph.0 != ft.).next() {
                    //     panic!("SWITCH TO {:?}", switch.graph.0)
                    //
                    // } else {
                    //     panic!("NO IDEA HOW TO RESOLVe")
                    // }
                    // Cant resolve from this subgraph
                }
                Some(current)
            }
            None => {
                if ft.join_field.is_empty() {
                    self.subgraph = Some(self.graph.subgraphs.iter().next().as_ref().unwrap().0);
                }
                if let Some(jf) = ft.join_field.iter().next() {
                    self.subgraph = Some(jf.graph.0);
                }
                None
            }
        };

        let mut afill: HashSet<T> = HashSet::default();
        for (name, arg) in &qf.arguments {
            if ft.arguments.get(name.as_ref()).is_none() {
                panic!("Unknown argument");
            }
            afill.insert(name.clone());
        };

        for (name, exp) in &ft.arguments {
            if exp.default_value.is_none() && !afill.contains(name) {
                panic!("Argument was not provided for field")
            }
        }

        let mut _typ = &ft.field_type;
        let mut segs = 0;

        // 10 nested arrays/nulls or nonnulls at the most
        for i in 0..16 {
            match _typ {
                Type::NamedType(typename) => {
                    if let Some(obj) = self.graph.types.get(typename) {
                        self.path.push(Seg::Item(qf.name.clone()));

                        let ns = self.node(&obj, qf.selection_set);
                        self.select.last_mut().unwrap().items.push(Selection::Field(Field {
                            position: Default::default(),
                            alias: qf.alias.clone(),
                            name: qf.name.clone(),
                            arguments: qf.arguments.clone(),
                            directives: qf.directives.clone(),
                            selection_set: ns,
                        }));
                        self.path.pop();
                        break;
                    } else {
                        panic!("Unknown: {:?}", typename)
                    }
                }
                Type::ListType(l) => {
                    self.path.push(Seg::Array);
                    segs += 1;
                    _typ = l.deref();
                }
                Type::NonNullType(t, ..) => {
                    _typ = t.deref();
                }
            }
        }
        for seg in 0..segs {
            self.path.pop();
        }
        self.subgraph = restore_sg;
    }

    pub fn fields(&mut self, typ: &TypeT<'static, &'static str>, set: SelectionSet<'a, T>) {
        for f in set.items {
            match (f, typ) {
                (Selection::Field(field), TypeT::Object(obj)) => {
                    self.field(obj, field)
                }
                (Selection::Field(field), TypeT::Union(obj)) => {
                    panic!("Can't select field on union directly, use fragments")
                }
                (Selection::InlineFragment(f), o @ TypeT::Object(obj)) => {
                    if let Some(TypeCondition::On(on, ..)) = &f.type_condition {
                        if on.as_ref() != obj.name {
                            panic!("Wrong type condition");
                        }
                    }
                    let selection_set = self.node(o, f.selection_set);
                    let sel = self.select.last_mut().unwrap();

                    sel.items.push(Selection::InlineFragment(InlineFragment {
                        position: Default::default(),
                        type_condition: f.type_condition.clone(),
                        directives: f.directives.clone(),
                        selection_set,
                    }))
                }
                (Selection::InlineFragment(f), TypeT::Union(u)) => {
                    let Some(TypeCondition::On(t, ..)) = &f.type_condition else {
                        panic!("Missing type condition for fragment with union")
                    };

                    let Some(tname) = u.types.iter().filter(|u| **u == t.as_ref()).next() else {
                        panic!("Could not find type from union")
                    };

                    let typ = self.graph.types.get(tname)
                        .expect("Could not resolve type");

                    let selection_set = self.node(typ, f.selection_set);
                    let sel = self.select.last_mut().unwrap();
                    sel.items.push(Selection::InlineFragment(InlineFragment {
                        position: Default::default(),
                        type_condition: f.type_condition.clone(),
                        directives: f.directives.clone(),
                        selection_set,
                    }))
                }
                _ => panic!("NamedFragments should have been resolved")
            }
        }
    }
    pub fn node(&mut self, typ: &TypeT<'static, &'static str>, set: SelectionSet<'a, T>) -> SelectionSet<'a, T> {
        self.select.push(SelectionSet {
            span: (Default::default(), Default::default()),
            items: vec![],
        });

        self.fields(typ, set);

        return self.select.pop().unwrap();
    }
}

type Variables = HashMap<String, serde_json::Value>;

pub fn supergraph<'a, T: Text<'a> + Hash>(parsed: Document<'a, T>) -> Graph<'a, T> {
    let mut schema = None;
    // let mut directives = HashMap::default();
    let mut itypes: HashMap<T, TypeDefinition<T>> = HashMap::default();

    for it in parsed.definitions.into_iter() {
        match it {
            Definition::SchemaDefinition(it) => schema = Some(it),
            Definition::SchemaExtension(_) => panic!("Schema extension not supported"),
            Definition::TypeDefinition(t) => {
                itypes.insert(t.name().clone(), t);
            }
            Definition::TypeExtension(e) => panic!("All extensions must be resolved"),
            Definition::DirectiveDefinition(d) => {
                // directives.insert(d.name.clone(), d);
            }
        }
    }

    let schema = schema.expect("Missing schema definition");

    let query = if let Some(ref query) = schema.query {
        itypes
            .iter()
            .filter(|v| v.0 == query)
            .map(|v| v.1.clone())
            .next()
            .map(|v| v.to_object().expect("Query must be an object"))
            .map(ObjectT::from)
    } else {
        None
    };

    let subscription = if let Some(ref sub) = schema.subscription {
        itypes
            .iter()
            .filter(|v| v.0 == sub)
            .map(|v| v.1.clone())
            .next()
            .map(|v| v.to_object().expect("Query must be an object"))
            .map(ObjectT::from)
    } else {
        None
    };

    let mutation = if let Some(ref mutation) = schema.mutation {
        itypes
            .iter()
            .filter(|v| v.0 == mutation)
            .map(|v| v.1.clone())
            .next()
            .map(|v| v.to_object().expect("Query must be an object"))
            .map(ObjectT::from)
    } else {
        None
    };

    let mut types: HashMap<_, TypeT<_>> = HashMap::from([
        (T::from("Int"), TypeT::Scalar(ScalarType::new(T::from("Int")))),
        (T::from("Float"), TypeT::Scalar(ScalarType::new(T::from("Float")))),
        (T::from("ID"), TypeT::Scalar(ScalarType::new(T::from("ID")))),
        (T::from("String"), TypeT::Scalar(ScalarType::new(T::from("String")))),
        (T::from("Boolean"), TypeT::Scalar(ScalarType::new(T::from("Boolean")))),
    ]);
    let mut enums: HashMap<_, _> = HashMap::default();
    let mut scalars: HashMap<_, _> = HashMap::default();
    let mut objects: HashMap<_, _> = HashMap::default();
    for (ident, typ) in &itypes {
        match typ {
            TypeDefinition::Scalar(s) => {
                scalars.insert(s.name.clone(), s.clone());
            }
            TypeDefinition::Object(o) => {
                types.insert(o.name.clone(), TypeT::Object(ObjectT::from(o.clone())));
            }
            TypeDefinition::Interface(_) => {}
            TypeDefinition::Union(u) => {
                types.insert(u.name.clone(), TypeT::Union(UnionT::from(u.clone())));
            }
            TypeDefinition::Enum(e) => {
                types.insert(e.name.clone(), TypeT::Enum(EnumT::from(e)));
                enums.insert(e.name.clone(), EnumT::from(e));
            }
            TypeDefinition::InputObject(_) => {}
        }
    }

    let mut graph = Graph {
        schema,
        // directives,
        // types,

        types,
        objects,
        enums,
        scalars,
        query,
        subscription,
        mutation,
        subgraphs: HashMap::default(),
    };

    let subs = itypes
        .get("join__Graph")
        .expect("Missing join__Graph enum")
        .as_enum()
        .expect("join__graph must be an enum");

    for val in &subs.values {
        let dir = val
            .directives
            .iter()
            .find(|d| d.name.as_ref() == "join__graph")
            .expect("Missing join_graph directive on join__Graph enum variant");

        let jg = JoinGraph::from(dir);

        graph.subgraphs.insert(
            val.name.clone(),
            SubGraph {
                key: val.name.clone(),
                name: jg.name,
                url: jg.url,
                _p: PhantomData,
            },
        );
    }


    graph
}
