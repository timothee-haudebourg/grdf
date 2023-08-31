use std::hash::Hash;

use linked_data::{
	GraphVisitor, LinkedDataResource, LinkedDataGraph, LinkedDataPredicateObjects, LinkedDataSubject,
	PredicateObjectsVisitor, ResourceInterpretation, SubjectVisitor,
};
use rdf_types::{Interpretation, Vocabulary};

use crate::{Dataset, Graph, GraphAccess, IdentityAccess};

pub trait DatasetAccess<D: ?Sized + Dataset>: GraphAccess<D::Graph> {
	fn subject_as_graph<'a>(
		&self,
		dataset: &'a D,
		subject: &'a D::Subject,
	) -> Option<&'a D::GraphLabel>;
}

impl<D: ?Sized + Dataset> DatasetAccess<D> for () {
	fn subject_as_graph<'a>(
		&self,
		_dataset: &'a D,
		_subject: &'a <D as Dataset>::Subject,
	) -> Option<&'a <D as Dataset>::GraphLabel> {
		None
	}
}

impl<D: ?Sized + Dataset<Subject = <D as Dataset>::Object, GraphLabel = <D as Dataset>::Subject>> DatasetAccess<D> for IdentityAccess {
	fn subject_as_graph<'a>(
		&self,
		_dataset: &'a D,
		subject: &'a <D as Dataset>::Subject,
	) -> Option<&'a <D as Dataset>::GraphLabel> {
		Some(subject)
	}
}

pub struct View<'a, D: ?Sized + Dataset, A> {
	dataset: &'a D,
	graph_label: Option<&'a D::GraphLabel>,
	graph: Option<&'a D::Graph>,
	subject: &'a D::Subject,
	access: A,
}

impl<'a, D: ?Sized + Dataset, A> View<'a, D, A> {
	pub fn new(
		dataset: &'a D,
		graph_label: Option<&'a D::GraphLabel>,
		graph: Option<&'a D::Graph>,
		subject: &'a D::Subject,
		access: A,
	) -> Self {
		Self {
			dataset,
			graph_label,
			graph,
			subject,
			access,
		}
	}
}

impl<'a, D: ?Sized + Dataset, A: DatasetAccess<D>, V: Vocabulary, I: Interpretation>
	LinkedDataSubject<V, I> for View<'a, D, A>
where
	D::Subject: Eq + Hash + LinkedDataResource<V, I>,
	D::Predicate: LinkedDataResource<V, I>,
	D::Object: LinkedDataResource<V, I>,
	D::GraphLabel: Eq + Hash + LinkedDataResource<V, I>,
{
	fn visit_subject<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
	where
		S: SubjectVisitor<V, I>,
	{
		let mut visited_subjects = im::HashSet::new();
		visited_subjects.insert(self.subject);

		let mut visited_graphs = im::HashSet::new();
		if let Some(graph_label) = self.graph_label {
			visited_graphs.insert(graph_label);
		}

		if let Some(graph) = self.graph {
			Subject::new(
				self.dataset,
				graph,
				self.subject,
				&self.access,
				&visited_subjects,
				&visited_graphs,
				true,
			)
			.visit(&mut serializer)?;
		}

		serializer.end()
	}
}

struct PredicateObjects<'d, 'v, D: ?Sized + Dataset, A> {
	dataset: &'d D,
	graph: &'d D::Graph,
	subject: &'d D::Subject,
	predicate: &'d D::Predicate,
	access: &'d A,
	visited_subjects: &'v im::HashSet<&'d D::Subject>,
	visited_graphs: &'v im::HashSet<&'d D::GraphLabel>,
}

impl<'d, 'v, D: ?Sized + Dataset, A: DatasetAccess<D>, V: Vocabulary, I: Interpretation>
	LinkedDataPredicateObjects<V, I> for PredicateObjects<'d, 'v, D, A>
where
	D::Subject: Eq + Hash + LinkedDataResource<V, I>,
	D::Predicate: LinkedDataResource<V, I>,
	D::Object: LinkedDataResource<V, I>,
	D::GraphLabel: Eq + Hash + LinkedDataResource<V, I>,
{
	fn visit_objects<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: PredicateObjectsVisitor<V, I>,
	{
		for object in self.graph.objects(self.subject, self.predicate) {
			visitor.object(&Object {
				dataset: self.dataset,
				graph: self.graph,
				object,
				access: self.access,
				visited_subjects: self.visited_subjects,
				visited_graphs: self.visited_graphs,
			})?;
		}

		visitor.end()
	}
}

impl<'a, 'v, D: ?Sized + Dataset, A, V: Vocabulary, I: Interpretation> LinkedDataResource<V, I>
	for Object<'a, 'v, D, A>
where
	D::Object: LinkedDataResource<V, I>,
{
	fn interpretation(
		&self,
		vocabulary: &mut V,
		interpretation: &mut I,
	) -> ResourceInterpretation<V, I> {
		self.object.interpretation(vocabulary, interpretation)
	}
}

struct Object<'a, 'v, D: ?Sized + Dataset, A> {
	dataset: &'a D,
	graph: &'a D::Graph,
	object: &'a D::Object,
	access: &'a A,
	visited_subjects: &'v im::HashSet<&'a D::Subject>,
	visited_graphs: &'v im::HashSet<&'a D::GraphLabel>,
}

impl<'a, 'v, D: ?Sized + Dataset, A: DatasetAccess<D>, V: Vocabulary, I: Interpretation>
	LinkedDataSubject<V, I> for Object<'a, 'v, D, A>
where
	D::Subject: Eq + Hash + LinkedDataResource<V, I>,
	D::Predicate: LinkedDataResource<V, I>,
	D::Object: LinkedDataResource<V, I>,
	D::GraphLabel: Eq + Hash + LinkedDataResource<V, I>,
{
	fn visit_subject<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: SubjectVisitor<V, I>,
	{
		if let Some(subject) = self.access.object_as_subject(self.graph, self.object) {
			let mut visited_subjects = self.visited_subjects.clone();
			let visit_predicates = visited_subjects.insert(subject).is_none();

			Subject::new(
				self.dataset,
				self.graph,
				subject,
				self.access,
				&visited_subjects,
				self.visited_graphs,
				visit_predicates,
			)
			.visit(&mut visitor)?;
		}

		visitor.end()
	}
}

struct Subject<'a, 'v, D: ?Sized + Dataset, A> {
	dataset: &'a D,
	graph: &'a D::Graph,
	subject: &'a D::Subject,
	access: &'a A,
	visited_subjects: &'v im::HashSet<&'a D::Subject>,
	visited_graphs: &'v im::HashSet<&'a D::GraphLabel>,
	visit_predicates: bool,
}

impl<'a, 'v, D: ?Sized + Dataset, A> Subject<'a, 'v, D, A>
where
	D::Subject: Eq + Hash,
	D::GraphLabel: Eq + Hash,
{
	fn new(
		dataset: &'a D,
		graph: &'a D::Graph,
		subject: &'a D::Subject,
		access: &'a A,
		visited_subjects: &'v im::HashSet<&'a D::Subject>,
		visited_graphs: &'v im::HashSet<&'a D::GraphLabel>,
		visit_predicates: bool,
	) -> Self {
		Self {
			dataset,
			graph,
			subject,
			access,
			visited_subjects,
			visited_graphs,
			visit_predicates,
		}
	}

	fn visit<V: Vocabulary, I: Interpretation, S>(&self, visitor: &mut S) -> Result<(), S::Error>
	where
		A: DatasetAccess<D>,
		S: SubjectVisitor<V, I>,
		D::Subject: LinkedDataResource<V, I>,
		D::Predicate: LinkedDataResource<V, I>,
		D::Object: LinkedDataResource<V, I>,
		D::GraphLabel: LinkedDataResource<V, I>,
	{
		if self.visit_predicates {
			for (predicate, _) in self.graph.predicates(self.subject) {
				visitor.predicate(
					predicate,
					&PredicateObjects {
						dataset: self.dataset,
						graph: self.graph,
						subject: self.subject,
						predicate,
						access: self.access,
						visited_subjects: &self.visited_subjects,
						visited_graphs: self.visited_graphs,
					},
				)?;
			}

			if let Some(label) = self.access.subject_as_graph(self.dataset, self.subject) {
				let mut visited_graphs = self.visited_graphs.clone();
				if visited_graphs.insert(label).is_none() {
					if let Some(graph) = self.dataset.graph(Some(label)) {
						visitor.graph(&GraphView {
							dataset: self.dataset,
							label,
							graph,
							access: self.access,
							visited_graphs: &visited_graphs,
						})?;
					}
				}
			}
		}

		Ok(())
	}
}

impl<'a, 'v, D: ?Sized + Dataset, A, V: Vocabulary, I: Interpretation> LinkedDataResource<V, I>
	for Subject<'a, 'v, D, A>
where
	D::Subject: LinkedDataResource<V, I>,
{
	fn interpretation(
		&self,
		vocabulary: &mut V,
		interpretation: &mut I,
	) -> ResourceInterpretation<V, I> {
		self.subject.interpretation(vocabulary, interpretation)
	}
}

impl<'a, 'v, D: ?Sized + Dataset, A: DatasetAccess<D>, V: Vocabulary, I: Interpretation>
	LinkedDataSubject<V, I> for Subject<'a, 'v, D, A>
where
	D::Subject: Eq + Hash + LinkedDataResource<V, I>,
	D::Predicate: LinkedDataResource<V, I>,
	D::Object: LinkedDataResource<V, I>,
	D::GraphLabel: Eq + Hash + LinkedDataResource<V, I>,
{
	fn visit_subject<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: SubjectVisitor<V, I>,
	{
		for (predicate, _) in self.graph.predicates(self.subject) {
			visitor.predicate(
				predicate,
				&PredicateObjects {
					dataset: self.dataset,
					graph: self.graph,
					subject: self.subject,
					predicate,
					access: self.access,
					visited_subjects: &self.visited_subjects,
					visited_graphs: &self.visited_graphs,
				},
			)?;
		}

		visitor.end()
	}
}

struct GraphView<'a, 'v, D: ?Sized + Dataset, A> {
	dataset: &'a D,
	label: &'a D::GraphLabel,
	graph: &'a D::Graph,
	access: &'a A,
	visited_graphs: &'v im::HashSet<&'a D::GraphLabel>,
}

impl<'a, 'v, D: ?Sized + Dataset, A, V: Vocabulary, I: Interpretation> LinkedDataResource<V, I>
	for GraphView<'a, 'v, D, A>
where
	D::GraphLabel: LinkedDataResource<V, I>,
{
	fn interpretation(
		&self,
		vocabulary: &mut V,
		interpretation: &mut I,
	) -> ResourceInterpretation<V, I> {
		self.label.interpretation(vocabulary, interpretation)
	}
}

impl<'a, 'v, D: ?Sized + Dataset, A: DatasetAccess<D>, V: Vocabulary, I: Interpretation>
	LinkedDataGraph<V, I> for GraphView<'a, 'v, D, A>
where
	D::Subject: Eq + Hash + LinkedDataResource<V, I>,
	D::Predicate: LinkedDataResource<V, I>,
	D::Object: LinkedDataResource<V, I>,
	D::GraphLabel: Eq + Hash + LinkedDataResource<V, I>,
{
	fn visit_graph<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: GraphVisitor<V, I>,
	{
		let mut visited_subjects = im::HashSet::new();
		for (subject, _) in self.graph.subjects() {
			visited_subjects.insert(subject);
		}

		for (subject, _) in self.graph.subjects() {
			visitor.subject(&Subject::new(
				self.dataset,
				self.graph,
				subject,
				self.access,
				&visited_subjects,
				self.visited_graphs,
				true,
			))?;
		}

		visitor.end()
	}
}
