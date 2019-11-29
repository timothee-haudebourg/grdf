#![feature(trait_alias)]

pub mod yaml;

use std::hash::Hash;
use std::collections::{HashMap, HashSet};

pub trait Entity = Hash + Eq;

pub struct Triplet<'a, T: 'a + Entity = Node, V: 'a = String> {
    pub subject: &'a T,
    pub prop: &'a T,
    pub object: &'a Object<T, V>
}

pub enum Object<T: Entity = Node, V = String> {
    Value(V),
    Entity(T)
}

pub struct Binding<T: Entity = Node, V = String> {
    prop: T,
    object: Object<T, V>
}

pub struct Graph<T: Entity = Node, V = String> {
    table: HashMap<T, HashSet<Binding<T, V>>>
}

impl<T: Entity, V> Default for Graph<T, V> {
    fn default() -> Graph<T, V> {
        Graph {
            table: HashMap::new()
        }
    }
}

pub struct DataSet<T: Entity = Node, V = String> {
    default: Graph<T, V>,
    nammed: HashMap<T, Graph<T, V>>
}

impl<T: Entity, V> Default for DataSet<T, V> {
    fn default() -> DataSet<T, V> {
        DataSet {
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
