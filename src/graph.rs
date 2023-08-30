use rdf_types::Triple;

mod view;

pub use view::*;

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

	/// Creates a view for the dataset, using the given subject from the given
	/// graph.
	fn view<'a, A>(&'a self, subject: &'a Self::Subject, access: A) -> GraphView<Self, A> {
		GraphView::new(self, subject, access)
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

pub trait GraphTake<
	T: ?Sized = <Self as Graph>::Subject,
	U: ?Sized = <Self as Graph>::Predicate,
	V: ?Sized = <Self as Graph>::Object,
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
