pub mod pass;

use axum::extract::State;
use axum::routing::post;
use axum::Json;
use clap::Parser;
use graphql_parser::query::{Directive, Document, Mutation, OperationDefinition, Query, Selection, Subscription, Text, Type};
use graphql_parser::schema::{Definition, DirectiveDefinition, EnumValue, SchemaDefinition, TypeDefinition};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::future::Future;
use std::marker::PhantomData;
use std::net::{Ipv4Addr, SocketAddrV4};
use std::path::PathBuf;
use std::sync::Arc;

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    #[arg(short, long)]
    pub schema: PathBuf,
}

#[derive(Debug, Clone)]
pub struct SubGraph<'a, T: Text<'a>> {
    pub key: T,
    pub name: String,
    pub url: String,
    _p: PhantomData<&'a ()>,
}

#[derive(Debug, Clone)]
struct Field<'a, T: Text<'a>> {
    graphs: Vec<T>,
    rtype: Type<'a, T>,
    args: Vec<(T, Type<'a, T>)>,
}

type Fields<'a, T> = HashMap<T, Field<'a, T>>;

#[derive(Debug, Clone)]
struct JoinType<'a, T> {
    graph: T,
    keys: String,
    _p: PhantomData<&'a ()>,
}

#[derive(Debug, Clone)]
struct Object<'a, T: Text<'a>> {
    graphs: Vec<JoinType<'a, T>>,
    fields: Fields<'a, T>,
}

#[derive(Debug, Clone)]
pub struct Graph<'a, T: Text<'a>> {
    pub schema: SchemaDefinition<'a, T>,
    pub directives: HashMap<T, DirectiveDefinition<'a, T>>,
    pub types: HashMap<T, TypeDefinition<'a, T>>,

    pub query: Option<TypeDefinition<'a, T>>,
    pub objects: HashMap<T, Object<'a, T>>,
    pub subgraphs: HashMap<T, SubGraph<'a, T>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Request {
    pub query: String,
    pub operation_name: Option<String>,
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub variables: HashMap<String, serde_json::Value>,
}

type Variables = HashMap<String, serde_json::Value>;
type StateGraph = State<&'static Graph<'static, &'static str>>;
type SGraph = &'static Graph<'static, &'static str>;

async fn handle_query<'a>(graph: SGraph, req: Query<'a, &'a str>, vars: &Variables) {
    let Some(graph) = &graph.query else {
        panic!("Query is not available in this supergraph");
    };

    for sel in &req.selection_set.items {
        match sel {
            Selection::Field(f) => {}
            Selection::FragmentSpread(_) => {}
            Selection::InlineFragment(_) => {}
        }
    }
    // panic!("{:#?}", query);
}

async fn handle_mutation<'a>(graph: SGraph, query: Mutation<'a, &'a str>, vars: &Variables) {}

async fn handle_subscription<'a>(
    graph: SGraph,
    query: Subscription<'a, &'a str>,
    vars: &Variables,
) {}

// #[axum::debug_handler]
async fn handler(State(graph): State<SGraph>, Json(req): Json<Request>) -> String {
    let query = req.query.as_str();

    let query = graphql_parser::parse_query::<&str>(query).expect("Bad query");

    match pass::frag::inline_fragments(query, req.operation_name.as_deref()) {
        OperationDefinition::SelectionSet(ss) => {
            handle_query(
                graph,
                Query {
                    position: ss.span.clone().0,
                    name: None,
                    variable_definitions: vec![],
                    directives: vec![],
                    selection_set: ss,
                },
                &req.variables,
            )
                .await
        }
        OperationDefinition::Query(q) => handle_query(graph, q, &req.variables).await,
        OperationDefinition::Mutation(m) => handle_mutation(graph, m, &req.variables).await,
        OperationDefinition::Subscription(s) => handle_subscription(graph, s, &req.variables).await,
    };
    Default::default()
}

#[tokio::main]
async fn main() {
    let args = Args::parse();
    let schema = Box::leak::<'static>(
        std::fs::read_to_string(args.schema)
            .expect("Could not read schema file")
            .into_boxed_str(),
    );

    let parsed =
        graphql_parser::parse_schema::<&'static str>(schema).expect("Could not parse schema");

    let mut schema = None;
    let mut directives = HashMap::default();
    let mut types = HashMap::default();

    for it in parsed.definitions.into_iter() {
        match it {
            Definition::SchemaDefinition(it) => schema = Some(it),
            Definition::SchemaExtension(_) => panic!("Schema extension not supported"),
            Definition::TypeDefinition(t) => {
                types.insert(*t.name(), t);
            }
            Definition::TypeExtension(e) => panic!("All extensions must be resolved"),
            Definition::DirectiveDefinition(d) => {
                directives.insert(d.name, d);
            }
        }
    }

    let schema = schema.expect("Missing schema definition");

    let query = if let Some(query) = schema.query {
        types.iter().filter(|v| *v.0 == query).map(|v| v.1.clone()).next()
    } else {
        None
    };

    let mut objects: HashMap<_, _> = HashMap::default();
    for (ident, typ) in &types {
        match typ {
            TypeDefinition::Scalar(_) => {}
            TypeDefinition::Object(o) => {
                let graphs = o.directives
                    .iter()
                    .filter(|i| i.name == "join__type")
                    .map(|d| {
                        let graph = d.arguments
                            .iter()
                            .filter(|a| a.0 == "graph")
                            .map(|a| {
                                *a.1.as_variable()
                                    .expect("join__type.graph must be an identifier")
                            })
                            .next()
                            .unwrap();
                        JoinType {
                            graph,
                            keys: "".to_string(),
                            _p: Default::default(),
                        }
                    })
                    .collect();

                objects.insert(o.name, Object {
                    graphs,
                    fields: Default::default(),
                });
            }
            TypeDefinition::Interface(_) => {}
            TypeDefinition::Union(_) => {}
            TypeDefinition::Enum(_) => {}
            TypeDefinition::InputObject(_) => {}
        }
    }


    let mut graph = Graph {
        schema,
        directives,
        types,

        query,
        objects,
        subgraphs: HashMap::default(),
    };

    let subs = graph
        .types
        .get("join__Graph")
        .expect("Missing join__Graph enum")
        .as_enum()
        .expect("join__graph must be an enum");

    for val in &subs.values {
        let dir = val
            .directives
            .iter()
            .find(|d| d.name == "join__graph")
            .expect("Missing join_graph directive on join__Graph enum variant");

        let name = dir
            .arguments
            .iter()
            .filter(|it| it.0 == "name")
            .next()
            .expect("Missing join__graph.name")
            .1
            .as_string()
            .expect("join__graph.name is supposed to be a string")
            .clone();

        let url = dir
            .arguments
            .iter()
            .filter(|it| it.0 == "url")
            .next()
            .expect("Missing join__graph.url")
            .1
            .as_string()
            .expect("join__graph.name is supposed to be a string")
            .clone();

        graph.subgraphs.insert(
            val.name,
            SubGraph {
                key: val.name,
                name,
                url,
                _p: PhantomData,
            },
        );
    }

    let graph: SGraph = Box::leak::<'static>(Box::new(graph));

    let router = axum::Router::new()
        .route("/", post(handler))
        .with_state(graph);

    let addr = SocketAddrV4::new(Ipv4Addr::UNSPECIFIED, 4000).into();

    axum::Server::bind(&addr)
        .serve(router.into_make_service())
        .await
        .expect("Server failed");
}
