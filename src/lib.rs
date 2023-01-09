//! The [Resource Description Framework (RDF)](https://en.wikipedia.org/wiki/Resource_Description_Framework)
//! is a powerful method for modeling data and knowledge
//! defined by the [World Wide Web Consortium (W3C)](https://www.w3.org/).
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
//! # use rdf_types::{Term, Quad};
//! # use grdf::HashDataset;
//! # let dataset: HashDataset<Term> = HashDataset::new();
//! for Quad(subject, predicate, object, graph) in dataset {
//!   // do something
//! }
//! ```
//!
//! Another way is to access each graph individually using `Dataset::graph`.
//! For a given graph, it is then possible to iterate through the triples of the
//! graph:
//!
//! ```rust
//! # use rdf_types::{Term, Triple};
//! # use grdf::HashDataset;
//! # let dataset: HashDataset<Term> = HashDataset::new();
//! # let id = None;
//! let graph = dataset.graph(id).unwrap();
//!
//! for Triple(subject, predicate, object) in graph {
//!   // do something
//! }
//! ```
//!
//! It is also possible to explore the graph logically, subject by subject,
//! predicate by predicate, object by object:
//!
//! ```rust
//! # use rdf_types::Term;
//! # use grdf::HashGraph;
//! # let graph: HashGraph<Term> = HashGraph::new();
//! // for each subject of the graph...
//! for (subject, predicates) in graph.subjects() {
//!   // for each predicate it is subject...
//!   for (predicate, objects) in predicates {
//!     // for each triple (subject, predicate, object)...
//!     for object in objects {
//!       // do something
//!     }
//!   }
//! }
//! ```
//!
//! ### Inserting new data
//!
//! Insertion can be done on `MutableDataset` implementations using
//! `MutableDataset::insert`:
//!
//! ```rust
//! # use rdf_types::{Term, Quad, BlankIdBuf};
//! # use grdf::HashDataset;
//! # let graph = None;
//! # let subject = Term::Blank(BlankIdBuf::from_u8(0));
//! # let predicate = Term::Blank(BlankIdBuf::from_u8(1));
//! # let object = Term::Blank(BlankIdBuf::from_u8(2));
//! let mut dataset: HashDataset<Term> = HashDataset::new();
//! dataset.insert(Quad(subject, predicate, object, graph));
//! ```
//!
//! Again it is possible to access each graph of the dataset mutably:
//!
//! ```rust
//! # use rdf_types::{Term, Triple, BlankIdBuf};
//! # use grdf::{MutableDataset, MutableGraph, HashDataset};
//! # let id = None;
//! # let subject = Term::Blank(BlankIdBuf::from_u8(0));
//! # let predicate = Term::Blank(BlankIdBuf::from_u8(1));
//! # let object = Term::Blank(BlankIdBuf::from_u8(2));
//! # let mut dataset: HashDataset<Term> = HashDataset::new();
//! let mut graph = dataset.graph_mut(id).unwrap();
//! graph.insert(Triple(subject, predicate, object));
//! ```
//!
//! ### Custom RDF types
//!
//! The types used to represent RDF subjects, predicate and objects are
//! parameters of the dataset. Anything can be used although they default to the
//! `rdf_types::Term` type that represents generic RDF nodes (blank nodes,
//! IRI-named nodes and literal values).
pub use rdf_types::{Quad, Triple};

pub mod btree_dataset;
pub mod hash_dataset;
pub mod macros;
pub mod utils;

#[cfg(feature = "meta")]
pub mod meta;

pub use btree_dataset::{BTreeDataset, BTreeGraph};
pub use hash_dataset::{HashDataset, HashGraph};

/// gRDF graph.
///
/// A graph is a collection of RDF triples.
/// It also defines a set of iterator to easily explore the graph.
pub trait Graph {
	type Subject;
	type Predicate;
	type Object;

	/// Triple iterators.
	type Triples<'a>: Iterator<
		Item = Triple<&'a Self::Subject, &'a Self::Predicate, &'a Self::Object>,
	> where
		Self: 'a;

	/// Graph subjects iterator.
	///
	/// Each subject is given with its associated predicates (and objects).
	type Subjects<'a>: Iterator<Item = (&'a Self::Subject, Self::Predicates<'a>)>
	where
		Self: 'a;

	/// Subject predicates iterator.
	///
	/// Iterate through all the predicates associated to a given subject.
	/// Each predicate is also given with the associated objects.
	type Predicates<'a>: Iterator<Item = (&'a Self::Predicate, Self::Objects<'a>)>
	where
		Self: 'a;

	/// Objects iterator.
	///
	/// Iterate through a set of objects.
	type Objects<'a>: Iterator<Item = &'a Self::Object>
	where
		Self: 'a;

	type PatternMatching<'a, 'p>: Iterator<
		Item = Triple<&'a Self::Subject, &'a Self::Predicate, &'a Self::Object>,
	> where
		Self: 'a,
		Self::Subject: 'p,
		Self::Predicate: 'p,
		Self::Object: 'p;

	/// Returns the number of triples in the graph.
	fn len(&self) -> usize;

	/// Checks if the graph is empty.
	fn is_empty(&self) -> bool {
		self.len() == 0
	}

	// Iterate through all the triples defined in the graph.
	fn triples(&self) -> Self::Triples<'_>;

	/// Iterate through all the subjects of the graph.
	fn subjects(&self) -> Self::Subjects<'_>;

	/// Iterate through all the predicates associated to the given subject.
	fn predicates(&self, subject: &Self::Subject) -> Self::Predicates<'_>;

	/// Iterate through all the objects associated to the given subject and
	/// predicate.
	fn objects(&self, subject: &Self::Subject, predicate: &Self::Predicate) -> Self::Objects<'_>;

	/// Checks if the given triple is defined in the graph.
	fn contains(&self, triple: Triple<&Self::Subject, &Self::Predicate, &Self::Object>) -> bool;

	/// Returns an iterator over all the triples matching the given pattern.
	#[allow(clippy::type_complexity)]
	fn pattern_matching<'p>(
		&self,
		pattern: Triple<
			Option<&'p Self::Subject>,
			Option<&'p Self::Predicate>,
			Option<&'p Self::Object>,
		>,
	) -> Self::PatternMatching<'_, 'p>;

	/// Returns any triple matching the given pattern.
	#[allow(clippy::type_complexity)]
	fn any_match(
		&self,
		pattern: Triple<Option<&Self::Subject>, Option<&Self::Predicate>, Option<&Self::Object>>,
	) -> Option<Triple<&Self::Subject, &Self::Predicate, &Self::Object>> {
		self.pattern_matching(pattern).next()
	}
}

/// Sized gRDF graph that can be converted into iterators.
///
/// Defines a set of iterators the graph can be consumed into.
pub trait SizedGraph: Graph + Sized {
	/// Consuming triples iterator.
	type IntoTriples: Iterator<
		Item = Triple<
			<Self as Graph>::Subject,
			<Self as Graph>::Predicate,
			<Self as Graph>::Object,
		>,
	>;

	/// Consuming subjects iterator.
	type IntoSubjects: Iterator<Item = (<Self as Graph>::Subject, Self::IntoPredicates)>;

	/// Consuming predicates iterator.
	type IntoPredicates: Iterator<Item = (<Self as Graph>::Predicate, Self::IntoObjects)>;

	/// Consuming objects iterator.
	type IntoObjects: Iterator<Item = <Self as Graph>::Object>;

	/// Consumes the graph and returns an iterator over its triples.
	fn into_triples(self) -> Self::IntoTriples;

	/// Consumes the graph and returns an iterator over its subjects.
	fn into_subjects(self) -> Self::IntoSubjects;

	/// Consumes the graph and returns an iterator over the predicates of the
	/// given subject.
	fn into_predicates(self, subject: &<Self as Graph>::Subject) -> Self::IntoPredicates;

	/// Consumes the graph and returns an iterator over the objects of the given
	/// subject and predicate.
	fn into_objects(
		self,
		subject: &<Self as Graph>::Subject,
		predicate: &<Self as Graph>::Predicate,
	) -> Self::IntoObjects;
}

/// Mutable gRDF graph.
pub trait MutableGraph: Graph {
	/// Insert the given triple into the graph.
	fn insert(
		&mut self,
		triple: Triple<
			<Self as Graph>::Subject,
			<Self as Graph>::Predicate,
			<Self as Graph>::Object,
		>,
	) -> bool;

	fn remove(
		&mut self,
		triple: Triple<
			&<Self as Graph>::Subject,
			&<Self as Graph>::Predicate,
			&<Self as Graph>::Object,
		>,
	);

	/// Absorb the given other graph.
	///
	/// Adds all the triples of `other` in the graph.
	fn absorb<
		G: SizedGraph<
			Subject = <Self as Graph>::Subject,
			Predicate = <Self as Graph>::Predicate,
			Object = <Self as Graph>::Object,
		>,
	>(
		&mut self,
		other: G,
	);
}

/// gRDF dataset.
///
/// A dataset is a collection of graphs.
/// It is made of a default graph and a collection of named graphs.
///
/// A dataset can also be seen as a collection of [`Quad`]s.
pub trait Dataset {
	type Subject;
	type Predicate;
	type Object;
	type GraphLabel;

	/// Type of graphs in the dataset.
	type Graph: Graph<Subject = Self::Subject, Predicate = Self::Predicate, Object = Self::Object>;

	/// Graph iterator.
	///
	/// Each graph is associated to its name (if any).
	type Graphs<'a>: Iterator<Item = (Option<&'a Self::GraphLabel>, &'a Self::Graph)>
	where
		Self: 'a;

	/// Quads iterator.
	type Quads<'a>: Iterator<
		Item = Quad<&'a Self::Subject, &'a Self::Predicate, &'a Self::Object, &'a Self::GraphLabel>,
	> where
		Self: 'a;

	type PatternMatching<'a, 'p>: Iterator<
		Item = Quad<&'a Self::Subject, &'a Self::Predicate, &'a Self::Object, &'a Self::GraphLabel>,
	> where
		Self: 'a,
		Self::Subject: 'p,
		Self::Predicate: 'p,
		Self::Object: 'p,
		Self::GraphLabel: 'p;

	/// Get the graph with the given name.
	/// Input `None` to get the default graph.
	fn graph(&self, id: Option<&Self::GraphLabel>) -> Option<&Self::Graph>;

	/// Get the default graph of the dataset.
	///
	/// This is the same as `graph(None)`.
	fn default_graph(&self) -> &Self::Graph {
		self.graph(None).unwrap()
	}

	/// Returns an iterator over the graphs of the dataset.
	fn graphs(&self) -> Self::Graphs<'_>;

	/// Returns an iterator over the quads of the dataset.
	fn quads(&self) -> Self::Quads<'_>;

	/// Returns the number of quads in the dataset.
	fn len(&self) -> usize {
		let mut len = 0;

		for (_, g) in self.graphs() {
			len += g.len()
		}

		len
	}

	/// Checks is the dataset is empty.
	fn is_empty(&self) -> bool {
		for (_, g) in self.graphs() {
			if !g.is_empty() {
				return false;
			}
		}

		true
	}

	/// Iterate through all the subjects of the given graph.
	fn subjects<'a>(
		&'a self,
		id: Option<&Self::GraphLabel>,
	) -> Option<<Self::Graph as Graph>::Subjects<'a>>
	where
		Self::Graph: 'a,
		Self::Subject: 'a,
		Self::Predicate: 'a,
		Self::Object: 'a,
	{
		self.graph(id).map(|graph| graph.subjects())
	}

	/// Iterate through all the predicates of the given subject of the given
	/// graph.
	fn predicates<'a>(
		&'a self,
		id: Option<&Self::GraphLabel>,
		subject: &Self::Subject,
	) -> Option<<Self::Graph as Graph>::Predicates<'a>>
	where
		Self::Graph: 'a,
		Self::Predicate: 'a,
		Self::Object: 'a,
	{
		self.graph(id).map(|graph| graph.predicates(subject))
	}

	/// Iterate through all the objects of the given subject and predicate of the
	/// given graph.
	fn objects<'a>(
		&'a self,
		id: Option<&Self::GraphLabel>,
		subject: &Self::Subject,
		predicate: &Self::Predicate,
	) -> Option<<Self::Graph as Graph>::Objects<'a>>
	where
		Self::Graph: 'a,
		Self::Object: 'a,
	{
		self.graph(id)
			.map(|graph| graph.objects(subject, predicate))
	}

	/// Checks if the given quad is defined in the dataset.
	#[allow(clippy::type_complexity)]
	fn contains(
		&self,
		Quad(s, p, o, g): Quad<&Self::Subject, &Self::Predicate, &Self::Object, &Self::GraphLabel>,
	) -> bool {
		match self.graph(g) {
			Some(g) => g.contains(Triple(s, p, o)),
			None => false,
		}
	}

	#[allow(clippy::type_complexity)]
	fn pattern_matching<'p>(
		&mut self,
		pattern: Quad<
			Option<&'p Self::Subject>,
			Option<&'p Self::Predicate>,
			Option<&'p Self::Object>,
			Option<&'p Self::GraphLabel>,
		>,
	) -> Self::PatternMatching<'_, 'p>;

	#[allow(clippy::type_complexity)]
	fn first_match(
		&mut self,
		pattern: Quad<
			Option<&Self::Subject>,
			Option<&Self::Predicate>,
			Option<&Self::Object>,
			Option<&Self::GraphLabel>,
		>,
	) -> Option<Quad<&Self::Subject, &Self::Predicate, &Self::Object, &Self::GraphLabel>> {
		self.pattern_matching(pattern).next()
	}
}

/// Sized gRDF dataset that can be converted into iterators.
pub trait SizedDataset: Dataset + Sized
where
	Self::Graph: SizedGraph,
{
	/// Consuming graphs iterator.
	type IntoGraphs: Iterator<Item = (Option<Self::GraphLabel>, Self::Graph)>;

	/// Consuming quads iterator.
	type IntoQuads: Iterator<
		Item = Quad<Self::Subject, Self::Predicate, Self::Object, Self::GraphLabel>,
	>;

	/// Consumes the dataset and returns the given graph.
	fn into_graph(self, id: Option<&Self::GraphLabel>) -> Option<Self::Graph>;

	/// Consumes the dataset and returns the default graph.
	fn into_default_graph(self) -> Self::Graph {
		self.into_graph(None).unwrap()
	}

	/// Consumes the dataset and returns an iterator over its graphs.
	fn into_graphs(self) -> Self::IntoGraphs;

	/// Consumes the dataset and returns an iterator over its quads.
	fn into_quads(self) -> Self::IntoQuads;

	/// Consumes the dataset and returns an iterator over the subjects of the
	/// given graph.
	fn into_subjects(
		self,
		id: Option<&Self::GraphLabel>,
	) -> Option<<Self::Graph as SizedGraph>::IntoSubjects> {
		self.into_graph(id).map(|graph| graph.into_subjects())
	}

	/// Consumes the dataset and returns an iterator over the predicates of the
	/// given subject of the given graph.
	fn into_predicates(
		self,
		id: Option<&Self::GraphLabel>,
		subject: &Self::Subject,
	) -> Option<<Self::Graph as SizedGraph>::IntoPredicates> {
		self.into_graph(id)
			.map(|graph| graph.into_predicates(subject))
	}

	/// Consumes the dataset and returns an iterator over the objects of the
	/// given subject and predicate of the given graph.
	fn into_objects(
		self,
		id: Option<&Self::GraphLabel>,
		subject: &Self::Subject,
		predicate: &Self::Predicate,
	) -> Option<<Self::Graph as SizedGraph>::IntoObjects> {
		self.into_graph(id)
			.map(|graph| graph.into_objects(subject, predicate))
	}
}

/// Mutable dataset.
pub trait MutableDataset: Dataset {
	/// Iterator over mutable graphs.
	type GraphsMut<'a>: Iterator<Item = (Option<&'a Self::GraphLabel>, &'a mut Self::Graph)>
	where
		Self: 'a;

	// type Drain<'a>: 'a + Iterator<Item = Quad<Self::Subject, Self::Predicate, Self::Object, Self::Graph>> where Self: 'a;

	// type DrainFilter<'a, F>: 'a + Iterator<Item = Quad<Self::Subject, Self::Predicate, Self::Object, Self::Graph>> where Self: 'a;

	// type DrainPatternMatching<'a>: 'a + Iterator<Item = Quad<Self::Subject, Self::Predicate, Self::Object, Self::Graph>> where Self: 'a;

	/// Get the given graph mutably.
	///
	/// Use the input `None` to get the default graph.
	///
	/// Note to implementors: the default graph should always exists.
	fn graph_mut(&mut self, id: Option<&Self::GraphLabel>) -> Option<&mut Self::Graph>;

	/// Get the default graph mutably.
	///
	/// Note to implementors: the default graph should always exists.
	fn default_graph_mut(&mut self) -> &mut Self::Graph {
		self.graph_mut(None).unwrap()
	}

	/// Returns an iterator over the (mutable) graphs of the dataset.
	fn graphs_mut(&mut self) -> Self::GraphsMut<'_>;

	/// Insert a graph in the dataset with the given name.
	///
	/// If a graph with the given name already exists,
	/// it is replaced and the previous graph definition is returned.
	fn insert_graph(&mut self, id: Self::GraphLabel, graph: Self::Graph) -> Option<Self::Graph>;

	/// Insert a quad in the dataset.
	fn insert(
		&mut self,
		quad: Quad<Self::Subject, Self::Predicate, Self::Object, Self::GraphLabel>,
	) -> bool;

	/// Remove a quad from the dataset.
	fn remove(
		&mut self,
		quad: Quad<&Self::Subject, &Self::Predicate, &Self::Object, &Self::GraphLabel>,
	);

	// fn drain(&mut self) -> Self::Drain<'_>;

	// fn drain_filter<F>(&mut self, pred: F) -> Self::DrainFilter<'_, F>;

	// fn drain_pattern_matching(
	// 	&mut self,
	// 	pattern: Quad<Option<&Self::Subject>, Option<&Self::Predicate>, Option<&Self::Object>, Option<&Self::GraphLabel>>
	// ) -> Self::DrainPatternMatching<'_>;

	/// Absorb the given other dataset.
	///
	/// Adds all the quads of `other` in the dataset.
	fn absorb<
		D: SizedDataset<
			Subject = Self::Subject,
			Predicate = Self::Predicate,
			Object = Self::Object,
			GraphLabel = Self::GraphLabel,
		>,
	>(
		&mut self,
		other: D,
	) where
		D::Graph: SizedGraph;
}

pub trait GraphTake<
	T = <Self as Graph>::Subject,
	U = <Self as Graph>::Predicate,
	V = <Self as Graph>::Object,
>: Graph
{
	#[allow(clippy::type_complexity)]
	fn take(
		&mut self,
		triple: Triple<&T, &U, &V>,
	) -> Option<Triple<Self::Subject, Self::Predicate, Self::Object>>;

	#[allow(clippy::type_complexity)]
	fn take_match(
		&mut self,
		triple: Triple<Option<&T>, Option<&U>, Option<&V>>,
	) -> Option<Triple<Self::Subject, Self::Predicate, Self::Object>>;
}

pub trait DatasetTake<
	T = <Self as Dataset>::Subject,
	U = <Self as Dataset>::Predicate,
	V = <Self as Dataset>::Object,
	W = <Self as Dataset>::GraphLabel,
>: MutableDataset where
	Self::Graph: GraphTake,
{
	#[allow(clippy::type_complexity)]
	fn take(
		&mut self,
		quad: Quad<&T, &U, &V, &W>,
	) -> Option<Quad<Self::Subject, Self::Predicate, Self::Object, Self::GraphLabel>>;

	#[allow(clippy::type_complexity)]
	fn take_match(
		&mut self,
		quad: Quad<Option<&T>, Option<&U>, Option<&V>, Option<&W>>,
	) -> Option<Quad<Self::Subject, Self::Predicate, Self::Object, Self::GraphLabel>>;
}
