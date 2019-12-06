#![feature(trait_alias)]

extern crate ordered_float;

use std::fmt;
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

impl<T: Entity + fmt::Display, V: fmt::Display> fmt::Display for Object<T, V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Entity(e) => e.fmt(f),
            Object::Value(v) => v.fmt(f)
        }
    }
}

// pub struct Binding<T: Entity = Node, V = Literal> {
//     prop: T,
//     object: Object<T, V>
// }

pub struct Graph<T: Entity = Node, V = Literal> {
    table: HashMap<T, HashMap<T, HashSet<Object<T, V>>>>,

    /// empty hash set used to produce empty iterators.
    dummy_object_set: HashSet<Object<T, V>>
}

impl<T: Entity, V: Hash + Eq> Default for Graph<T, V> {
    fn default() -> Graph<T, V> {
        Graph {
            table: HashMap::new(),
            dummy_object_set: HashSet::new()
        }
    }
}

impl<T: Entity, V: Hash + Eq> Graph<T, V> {
    pub fn subjects(&self) -> impl Iterator<Item = &T> {
        self.table.keys()
    }

    pub fn get(&self, subject: &T, predicate: &T) -> Option<&Object<T, V>> {
        self.get_all(subject, predicate).next()
    }

    pub fn get_all(&self, subject: &T, predicate: &T) -> impl Iterator<Item = &Object<T, V>> {
        if let Some(subject_table) = self.table.get(subject) {
            if let Some(objects) = subject_table.get(predicate) {
                objects.iter()
            } else {
                self.dummy_object_set.iter()
            }
        } else {
            self.dummy_object_set.iter()
        }
    }

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
    pub fn default_graph(&self) -> &Graph<T, V> {
        &self.default
    }

    pub fn nammed_graph(&self, name: &T) -> Option<&Graph<T, V>> {
        self.nammed.get(name)
    }

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

impl<T: Entity, V: Hash + Eq> Default for Dataset<T, V> {
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

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Node::Anonymous(id) => write!(f, "_:{}", id),
            Node::Nammed(id) => id.fmt(f)
        }
    }
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

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::String(str, Some(tag)) => write!(f, "\"{}\"{}", str, tag),
            Literal::String(str, None) => write!(f, "\"{}\"", str),
            Literal::Int(i) => i.fmt(f),
            Literal::Float(d) => d.fmt(f),
            Literal::Bool(true) => write!(f, "true"),
            Literal::Bool(false) => write!(f, "false")
        }
    }
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

impl fmt::Display for Tag {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Tag::Lang(lang) => write!(f, "@{}", lang),
            Tag::Iri(iri) => write!(f, "^^<{}>", iri)
        }
    }
}
