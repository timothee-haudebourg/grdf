//! The [Resource Description Framework (RDF)](https://en.wikipedia.org/wiki/Resource_Description_Framework)
//! is a powerful method for modeling data and knowledge
//! defined by the [World Wide Web Consorsium (W3C)](https://www.w3.org/).
//! A RDF dataset consists in a collection of graphs connecting nodes, values
//! and predicates. This crate provides traits and implementations of
//! [Generalized RDF (gRDF)](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#section-generalized-rdf)
//! where nodes, values and predicates have the same representation.
//!
//! Note that this crates requires the nightly compiler.
//! It needs Generic Associated Typed (GAT) to work properly,
//! which are [still being implemented](https://github.com/rust-lang/rust/issues/44265).
//!
//! ## Basic usage
//!
//! ### Exploring a dataset
//!
//! Each `Dataset` implementation provides many iterators to explore the data.
//! One simple way is to iterate through the quad of the dataset:
//!
//! ```rust
//! # use grdf::{Term, Dataset, Quad, HashDataset};
//! # let dataset: HashDataset<Term> = HashDataset::new();
//! for Quad(graph, subject, predicate, object) in dataset.quads() {
//! 	// do something
//! }
//! ```
//!
//! Another way is to access each graph individually using `Dataset::graph`.
//! For a given graph, it is then possible to iterate through the triples of the
//! graph:
//!
//! ```rust
//! # use grdf::{Term, Dataset, Graph, Triple, HashDataset};
//! # let dataset: HashDataset<Term> = HashDataset::new();
//! # let id = None;
//! let graph = dataset.graph(id).unwrap();
//!
//! for Triple(subject, predicate, object) in graph.triples() {
//! 	// do something
//! }
//! ```
//!
//! It is also possible to explore the graph logically, subject by subject,
//! predicate by predicate, object by object:
//!
//! ```rust
//! # use grdf::{Term, Graph, HashGraph};
//! # let graph: HashGraph<Term> = HashGraph::new();
//! // for each subject of the graph...
//! for (subject, predicates) in graph.subjects() {
//! 	// for each predicate it is subject...
//! 	for (predicate, objects) in predicates {
//! 		// for each triple (subject, predicate, object)...
//! 		for object in objects {
//! 			// do something
//! 		}
//! 	}
//! }
//! ```
//!
//! ### Inserting new data
//!
//! Insertion can be done on `MutableDataset` implementations using
//! `MutableDataset::insert`:
//!
//! ```rust
//! # use grdf::{Term, MutableDataset, Quad, HashDataset};
//! # let graph = None;
//! # let subject = Term::Blank(0);
//! # let predicate = Term::Blank(1);
//! # let object = Term::Blank(2);
//! let mut dataset: HashDataset<Term> = HashDataset::new();
//! dataset.insert(Quad(graph, subject, predicate, object));
//! ```
//!
//! Again it is possible to access each graph of the dataset mutably:
//!
//! ```rust
//! # use grdf::{Term, MutableDataset, MutableGraph, Triple, HashDataset};
//! # let id = None;
//! # let subject = Term::Blank(0);
//! # let predicate = Term::Blank(1);
//! # let object = Term::Blank(2);
//! # let mut dataset: HashDataset<Term> = HashDataset::new();
//! let mut graph = dataset.graph_mut(id).unwrap();
//! graph.insert(Triple(subject, predicate, object));
//! ```
//!
//! ### Custom node type
//!
//! The type used to represent RDF nodes (subjects, predicate and objects) is a
//! parameter of the dataset. Anything can be used although this crate provide a
//! default `Term` type that represents generic RDF nodes (blank nodes,
//! IRI-named nodes and literal values).

#![feature(generic_associated_types)]
use std::fmt;

pub mod hash_dataset;
mod term;

pub use hash_dataset::{HashDataset, HashGraph};
pub use term::*;

/// gRDF triple.
pub struct Triple<T>(pub T, pub T, pub T);

impl<T> Triple<T> {
	pub fn subject(&self) -> &T { &self.0 }

	pub fn predicate(&self) -> &T { &self.1 }

	pub fn object(&self) -> &T { &self.2 }

	pub fn into_parts(self) -> (T, T, T) { (self.0, self.1, self.2) }
}

impl<T: fmt::Display> fmt::Display for Triple<T> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{} {} {}", self.0, self.1, self.2)
	}
}

/// Graph iterators.
///
/// This trait is needed because currently the Rust compiler has trouble
/// correctly handling generic associated types.
/// When `rustc` will be ready, all the types defined in this trait will be
/// defined in the `Graph` trait. Until then we need it.
pub trait Iter<'a, T: 'a> {
	/// Triple iterators.
	type Triples: Iterator<Item = Triple<&'a T>>;

	/// Graph subjects iterator.
	///
	/// Each subject is given with its associated predicates (and objects).
	type Subjects: Iterator<Item = (&'a T, Self::Predicates)>;

	/// Subject predicates iterator.
	///
	/// Iterate through all the predicates associated to a given subject.
	/// Each predicate is also given with the associated objects.
	type Predicates: Iterator<Item = (&'a T, Self::Objects)>;

	/// Objects iterator.
	///
	/// Iterate through a set of objects.
	type Objects: Iterator<Item = &'a T>;
}

/// gRDF graph.
///
/// A graph is a collection of RDF triples.
/// It also defines a set of iterator to easily explore the graph.
pub trait Graph<T = crate::Term> {
	/// Iterators.
	type Iter<'a>: Iter<'a, T>;

	// Iterate through all the triples defined in the graph.
	fn triples<'a>(&'a self) -> <Self::Iter<'a> as Iter<'a, T>>::Triples
	where
		T: 'a;

	/// Iterate through all the subjects of the graph.
	fn subjects<'a>(&'a self) -> <Self::Iter<'a> as Iter<'a, T>>::Subjects
	where
		T: 'a;

	/// Iterate through all the predicates associated to the given subject.
	fn predicates<'a>(&'a self, subject: &T) -> <Self::Iter<'a> as Iter<'a, T>>::Predicates
	where
		T: 'a;

	/// Iterate through all the objects associated to the given subject and
	/// predicate.
	fn objects<'a>(
		&'a self,
		subject: &T,
		predicate: &T,
	) -> <Self::Iter<'a> as Iter<'a, T>>::Objects
	where
		T: 'a;

	/// Checks if the given triple is defined in the graph.
	fn contains(&self, triple: Triple<&T>) -> bool;
}

/// Sized gRDF graph that can be converted into iterators.
///
/// Defines a set of iterators the graph can be consumed into.
pub trait SizedGraph<T = crate::Term>: Graph<T> + Sized {
	/// Consuming triples iterator.
	type IntoTriples: Iterator<Item = Triple<T>>;

	/// Consuming subjects iterator.
	type IntoSubjects: Iterator<Item = (T, Self::IntoPredicates)>;

	/// Consuming predicates iterator.
	type IntoPredicates: Iterator<Item = (T, Self::IntoObjects)>;

	/// Consuming objects iterator.
	type IntoObjects: Iterator<Item = T>;

	/// Consumes the graph and returns an iterator over its triples.
	fn into_triples(self) -> Self::IntoTriples;

	/// Consumes the graph and returns an iterator over its subjects.
	fn into_subjects(self) -> Self::IntoSubjects;

	/// Consumes the graph and returns an iterator over the predicates of the
	/// given subject.
	fn into_predicates(self, subject: &T) -> Self::IntoPredicates;

	/// Consumes the graph and returns an iterator over the objects of the given
	/// subject and predicate.
	fn into_objects(self, subject: &T, predicate: &T) -> Self::IntoObjects;
}

/// Mutable gRDF graph.
pub trait MutableGraph<T = crate::Term>: Graph<T> {
	/// Insert the given triple into the graph.
	fn insert(&mut self, triple: Triple<T>);

	/// Absorb the given other graph.
	///
	/// Adds all the triples of `other` in the graph.
	fn absorb<G: SizedGraph<T>>(&mut self, other: G);
}

/// gRDF quad.
pub struct Quad<T>(pub Option<T>, pub T, pub T, pub T);

impl<T> Quad<T> {
	pub fn graph(&self) -> Option<&T> { self.0.as_ref() }

	pub fn subject(&self) -> &T { &self.1 }

	pub fn predicate(&self) -> &T { &self.2 }

	pub fn object(&self) -> &T { &self.3 }

	pub fn into_parts(self) -> (Option<T>, T, T, T) { (self.0, self.1, self.2, self.3) }
}

impl<T: fmt::Display> fmt::Display for Quad<T> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self.graph() {
			Some(graph) => write!(f, "{} {} {} {}", self.1, self.2, self.3, graph),
			None => write!(f, "{} {} {}", self.1, self.2, self.3)
		}
	}
}

/// gRDF dataset.
///
/// A dataset is a collection of graphs.
/// It is made of a default graph and a collection of named graphs.
///
/// A dataset can also be seen as a collection of [`Quad`]s.
pub trait Dataset<T = crate::Term> {
	/// Type of graphs in the dataset.
	type Graph: Graph<T>;

	/// Graph iterator.
	///
	/// Each graph is associated to its name (if any).
	type Graphs<'a>: Iterator<Item = (Option<&'a T>, &'a Self::Graph)>;

	/// Quads iterator.
	type Quads<'a>: Iterator<Item = Quad<&'a T>>;

	/// Get the graph with the given name.
	/// Input `None` to get the default graph.
	///
	/// Note to implementors: the default graph should always exists.
	fn graph(&self, id: Option<&T>) -> Option<&Self::Graph>;

	/// Get the default graph of the dataset.
	///
	/// This is the same as `graph(None)`.
	///
	/// Note to implementors: the default graph should always exists.
	fn default_graph(&self) -> &Self::Graph { self.graph(None).unwrap() }

	/// Returns an iterator over the graphs of the dataset.
	fn graphs<'a>(&'a self) -> Self::Graphs<'a>;

	/// Returns an iterator over the quads of the dataset.
	fn quads<'a>(&'a self) -> Self::Quads<'a>;

	/// Iterate through all the subjects of the given graph.
	fn subjects<'a>(
		&'a self,
		id: Option<&T>,
	) -> Option<<<Self::Graph as Graph<T>>::Iter<'a> as Iter<'a, T>>::Subjects> {
		match self.graph(id) {
			Some(graph) => Some(graph.subjects()),
			None => None,
		}
	}

	/// Iterate through all the predicates of the given subject of the given
	/// graph.
	fn predicates<'a>(
		&'a self,
		id: Option<&T>,
		subject: &T,
	) -> Option<<<Self::Graph as Graph<T>>::Iter<'a> as Iter<'a, T>>::Predicates> {
		match self.graph(id) {
			Some(graph) => Some(graph.predicates(subject)),
			None => None,
		}
	}

	/// Iterate through all the objects of the given subect and predicate of the
	/// given graph.
	fn objects<'a>(
		&'a self,
		id: Option<&T>,
		subject: &T,
		predicate: &T,
	) -> Option<<<Self::Graph as Graph<T>>::Iter<'a> as Iter<'a, T>>::Objects> {
		match self.graph(id) {
			Some(graph) => Some(graph.objects(subject, predicate)),
			None => None,
		}
	}
}

/// Sized gRDF dataset that can be converted into iterators.
pub trait SizedDataset<T = crate::Term>: Dataset<T> + Sized
where
	Self::Graph: SizedGraph<T>,
{
	/// Consuming graphs iterator.
	type IntoGraphs: Iterator<Item = (Option<T>, Self::Graph)>;

	/// Consuming quads iterator.
	type IntoQuads: Iterator<Item = Quad<T>>;

	/// Consumes the dataset and returns the given graph.
	fn into_graph(self, id: Option<&T>) -> Option<Self::Graph>;

	/// Consumes the dataset and returns the default graph.
	fn into_default_graph(self) -> Self::Graph { self.into_graph(None).unwrap() }

	/// Consumes the dataset and returns an iterator over its graphs.
	fn into_graphs(self) -> Self::IntoGraphs;

	/// Consumes the dataset and returns an iterator over its quads.
	fn into_quads(self) -> Self::IntoQuads;

	/// Consumes the dataset and returns an iterator over the subjects of the
	/// given graph.
	fn subjects(self, id: Option<&T>) -> Option<<Self::Graph as SizedGraph<T>>::IntoSubjects> {
		match self.into_graph(id) {
			Some(graph) => Some(graph.into_subjects()),
			None => None,
		}
	}

	/// Consumes the dataset and returns an iterator over the predicates of the
	/// given subject of the given graph.
	fn predicates(
		self,
		id: Option<&T>,
		subject: &T,
	) -> Option<<Self::Graph as SizedGraph<T>>::IntoPredicates> {
		match self.into_graph(id) {
			Some(graph) => Some(graph.into_predicates(subject)),
			None => None,
		}
	}

	/// Consumes the dataset and returns an iterator over the objects of the
	/// given subject and predicate of the given graph.
	fn objects(
		self,
		id: Option<&T>,
		subject: &T,
		predicate: &T,
	) -> Option<<Self::Graph as SizedGraph<T>>::IntoObjects> {
		match self.into_graph(id) {
			Some(graph) => Some(graph.into_objects(subject, predicate)),
			None => None,
		}
	}
}

/// Mutable dataset.
pub trait MutableDataset<T = crate::Term>: Dataset<T> {
	/// Iterator over mutable graphs.
	type GraphsMut<'a>: Iterator<Item = (Option<&'a T>, &'a mut Self::Graph)>;

	/// Get the given graph mutabily.
	///
	/// Use the input `None` to get the default graph.
	///
	/// Note to implementors: the default graph should always exists.
	fn graph_mut(&mut self, id: Option<&T>) -> Option<&mut Self::Graph>;

	/// Get the default graph mutabily.
	///
	/// Note to implementors: the default graph should always exists.
	fn default_graph_mut(&mut self) -> &mut Self::Graph { self.graph_mut(None).unwrap() }

	/// Returns an iterator over the (mutable) graphs of the dataset.
	fn graphs_mut<'a>(&'a mut self) -> Self::GraphsMut<'a>;

	/// Insert a graph in the dataset with the given name.
	///
	/// If a graph with the given name already exists,
	/// it is replaced and the previous graph definition is returned.
	fn insert_graph(&mut self, id: T, graph: Self::Graph) -> Option<Self::Graph>;

	/// Insert a quad in the dataset.
	fn insert(&mut self, quad: Quad<T>);

	/// Absorb the given other dataset.
	///
	/// Adds all the quads of `other` in the dataset.
	fn absorb<D: SizedDataset<T>>(&mut self, other: D)
	where
		D::Graph: SizedGraph<T>;
}
