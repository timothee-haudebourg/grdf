#![feature(trait_alias)]

extern crate ordered_float;

pub mod yaml;

use std::hash::Hash;
use std::collections::{HashMap, HashSet};
use ordered_float::NotNan;

pub trait Entity = Hash + Eq;

pub struct Triplet<'a, T: 'a + Entity = Node, V: 'a = Literal> {
    pub subject: &'a T,
    pub prop: &'a T,
    pub object: &'a Object<T, V>
}

#[derive(Hash, PartialEq, Eq)]
pub enum Object<T: Entity = Node, V = Literal> {
    Value(V),
    Entity(T)
}

// pub struct Binding<T: Entity = Node, V = Literal> {
//     prop: T,
//     object: Object<T, V>
// }

pub struct Graph<T: Entity = Node, V = Literal> {
    table: HashMap<T, HashMap<T, HashSet<Object<T, V>>>>
}

impl<T: Entity, V> Default for Graph<T, V> {
    fn default() -> Graph<T, V> {
        Graph {
            table: HashMap::new()
        }
    }
}

impl<T: Entity, V: Hash + Eq> Graph<T, V> {
    pub fn add(&mut self, subject: T, predicate: T, object: Object<T, V>) {
        match self.table.get_mut(&subject) {
            Some(bindings) => {
                match bindings.get_mut(&predicate) {
                    Some(objects) => {
                        objects.insert(object);
                    },
                    None => {
                        let mut objects = HashSet::new();
                        objects.insert(object);
                        bindings.insert(predicate, objects);
                    }
                }
            },
            None => {
                let mut bindings = HashMap::new();
                let mut objects = HashSet::new();
                objects.insert(object);
                bindings.insert(predicate, objects);
                self.table.insert(subject, bindings);
            }
        }
    }
}

pub struct Dataset<T: Entity = Node, V = Literal> {
    default: Graph<T, V>,
    nammed: HashMap<T, Graph<T, V>>
}

impl<T: Entity, V: Hash + Eq> Dataset<T, V> {
    pub fn add(&mut self, graph_name: Option<T>, subject: T, predicate: T, object: Object<T, V>) {
        match graph_name {
            Some(name) => {
                match self.nammed.get_mut(&name) {
                    Some(g) => g.add(subject, predicate, object),
                    None => {
                        let mut g = Graph::default();
                        g.add(subject, predicate, object);
                        self.nammed.insert(name, g);
                    }
                }
            },
            None => {
                self.default.add(subject, predicate, object)
            }
        }
    }
}

impl<T: Entity, V> Default for Dataset<T, V> {
    fn default() -> Dataset<T, V> {
        Dataset {
            default: Graph::default(),
            nammed: HashMap::new()
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub enum Node {
    Anonymous(usize),
    Nammed(String)
}

/**
 * RDF literals.
 */
#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub enum Literal {
    String(String, Option<Tag>),
    Int(i64),
    Float(NotNan<f64>),
    Bool(bool)
}

#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub enum Tag {
    /**
     * Language tag.
     */
    Lang(String),

    /**
     * IRI tag.
     */
    Iri(String)
}
