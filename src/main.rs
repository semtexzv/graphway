pub mod fed;
pub mod pass;

use crate::fed::{Graph, ObjectT, Plan, Planner, TypeT};
use axum::extract::State;
use axum::routing::post;
use axum::Json;
use clap::Parser;
use graphql_parser::query::{
    Mutation, OperationDefinition, Query, Selection, SelectionSet, Subscription, Text, Type,
};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, LinkedList};
use std::net::{Ipv4Addr, SocketAddrV4};
use std::path::PathBuf;
use graphql_parser::parse_query;

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    #[arg(short, long)]
    pub schema: PathBuf,
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

pub enum Op {}

struct ResolveKey<'a, T: Text<'a>> {
    graph: &'static str,
    keys: graphql_parser::query::SelectionSet<'a, T>,
}

async fn handle_query<'a>(graph: SGraph, req: Query<'a, &'a str>, vars: &Variables) {
    let Some(qtype) = &graph.query else {
        panic!("Query is not available in this supergraph");
    };

    let mut exec: HashMap<ResolveKey<'a, &'a str>, SelectionSet<'static, String>> =
        Default::default();

    for sel in &req.selection_set.items {
        match sel {
            Selection::Field(fsel) => {
                let Some(fdef) = qtype.fields.get(fsel.name) else {
                    panic!("Did not find field")
                };

                // Recursively
                // Go through every field,
                // IF - it is in current subgraph, add it to subgraph query
                // IF it is in other subgraph, and it is resolvable there (
                // create a subsequent request keyed on (subgraph, key fields)
                // Add that field to that request

                match &fdef.field_type {
                    Type::NamedType(n) => {
                        // if let Some(obj) = graph.objects.get()
                        // let obj = graph.types.get(n).expect("Unknown type");
                    }
                    Type::ListType(i) => {}
                    Type::NonNullType(n, _) => {}
                }
            }
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

    let graph = fed::supergraph(parsed);

    let graph = Box::leak(Box::new(graph));

    let mut planner = Planner::<&str>::new(graph, graph.query.as_ref().unwrap());

    let q = parse_query::<&str>(include_str!("../query.gql")).unwrap();
    let q = pass::frag::inline_fragments(q, None);
    let done = match q {
        OperationDefinition::Query(q) => {
            let typ = TypeT::Object(graph.query.clone().unwrap());
            planner.node(&typ, q.selection_set)
        },
        _ => panic!("aa")
    };

    panic!("DONE: {}", done.to_string());


    let router = axum::Router::new()
        .route("/", post(handler))
        .with_state(graph);

    let addr = SocketAddrV4::new(Ipv4Addr::UNSPECIFIED, 4000).into();

    axum::Server::bind(&addr)
        .serve(router.into_make_service())
        .await
        .expect("Server failed");
}
