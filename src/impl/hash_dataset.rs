//! Dataset implementation based on `HashMap` and `HashSet`.
use crate::View;
use crate::{utils::HashBijection, Quad, Triple};
use derivative::Derivative;
use hashbrown::{Equivalent, HashMap, HashSet};
use rdf_types::{AsRdfTerm, FromBlankId, IntoBlankId, MaybeBlankId};
use std::fmt;
use std::hash::Hash;

mod graph;

pub use graph::*;

#[derive(Derivative, Clone)]
#[derivative(PartialEq(bound = "S: Eq + Hash, P: Eq + Hash, O: Eq + Hash, G: Eq + Hash"))]
#[derivative(Eq(bound = "S: Eq + Hash, P: Eq + Hash, O: Eq + Hash, G: Eq + Hash"))]
#[derivative(Default(bound = ""))]
pub struct HashDataset<S = rdf_types::Term, P = S, O = S, G = S> {
	default: HashGraph<S, P, O>,
	named: HashMap<G, HashGraph<S, P, O>>,
}

impl<S, P, O, G> HashDataset<S, P, O, G> {
	pub fn new() -> Self {
		Self::default()
	}

	pub fn graph<W: ?Sized + Equivalent<G> + Hash>(
		&self,
		id: Option<&W>,
	) -> Option<&HashGraph<S, P, O>>
	where
		G: Eq + Hash,
	{
		match id {
			Some(id) => self.named.get(id),
			None => Some(&self.default),
		}
	}

	#[allow(clippy::type_complexity)]
	pub fn graph_entry<W: ?Sized + Equivalent<G> + Hash>(
		&self,
		id: Option<&W>,
	) -> Option<(Option<&G>, &HashGraph<S, P, O>)>
	where
		G: Eq + Hash,
	{
		match id {
			Some(id) => self.named.get_key_value(id).map(|(k, v)| (Some(k), v)),
			None => Some((None, &self.default)),
		}
	}

	pub fn graphs(&self) -> Graphs<'_, S, P, O, G> {
		Graphs {
			default: Some(&self.default),
			it: self.named.iter(),
		}
	}

	pub fn quads(&self) -> Quads<'_, S, P, O, G> {
		Quads {
			graphs: self.graphs(),
			graph: None,
			triples: None,
		}
	}

	pub fn graph_mut<W: ?Sized + Equivalent<G> + Hash>(
		&mut self,
		id: Option<&W>,
	) -> Option<&mut HashGraph<S, P, O>>
	where
		G: Eq + Hash,
	{
		match id {
			Some(id) => self.named.get_mut(id),
			None => Some(&mut self.default),
		}
	}

	pub fn graphs_mut(&mut self) -> GraphsMut<S, P, O, G> {
		GraphsMut {
			default: Some(&mut self.default),
			it: self.named.iter_mut(),
		}
	}

	pub fn insert_graph(&mut self, id: G, graph: HashGraph<S, P, O>) -> Option<HashGraph<S, P, O>>
	where
		G: Eq + Hash,
	{
		self.named.insert(id, graph)
	}

	pub fn into_graph(mut self, id: Option<&G>) -> Option<HashGraph<S, P, O>>
	where
		G: Eq + Hash,
	{
		match id {
			Some(id) => self.named.remove(id),
			None => Some(self.default),
		}
	}

	pub fn into_graphs(self) -> IntoGraphs<S, P, O, G> {
		IntoGraphs {
			default: Some(self.default),
			it: self.named.into_iter(),
		}
	}

	pub fn into_quads(self) -> IntoQuads<S, P, O, G> {
		IntoQuads {
			graphs: self.into_graphs(),
			graph: None,
			triples: None,
		}
	}
}

impl<S: Eq + Hash, P: Eq + Hash, O: Eq + Hash, G: Eq + Hash> HashDataset<S, P, O, G> {
	pub fn insert(&mut self, quad: Quad<S, P, O, G>) -> bool {
		let (subject, predicate, object, graph_name) = quad.into_parts();
		match self.graph_mut(graph_name.as_ref()) {
			Some(g) => g.insert(Triple(subject, predicate, object)),
			None => {
				let mut g = HashGraph::new();
				g.insert(Triple(subject, predicate, object));
				self.insert_graph(graph_name.unwrap(), g);
				true
			}
		}
	}

	pub fn remove<
		T: ?Sized + Equivalent<S> + Hash,
		U: ?Sized + Equivalent<P> + Hash,
		V: ?Sized + Equivalent<O> + Hash,
		W: ?Sized + Equivalent<G> + Hash,
	>(
		&mut self,
		Quad(s, p, o, g): Quad<&T, &U, &V, &W>,
	) {
		if let Some(graph) = self.graph_mut(g) {
			graph.remove(Triple(s, p, o))
		}
	}

	pub fn remove_graph<W: ?Sized + Equivalent<G> + Hash>(
		&mut self,
		g: &W,
	) -> Option<HashGraph<S, P, O>> {
		self.named.remove(g)
	}

	pub fn take<
		T: ?Sized + Equivalent<S> + Hash,
		U: ?Sized + Equivalent<P> + Hash,
		V: ?Sized + Equivalent<O> + Hash,
		W: ?Sized + Equivalent<G> + Hash,
	>(
		&mut self,
		Quad(s, p, o, g): Quad<&T, &U, &V, &W>,
	) -> Option<Quad<S, P, O, G>>
	where
		S: Clone,
		P: Clone,
		G: Clone,
	{
		let graph = self.graph_mut(g)?;
		let Triple(s, p, o) = graph.take(Triple(s, p, o))?;

		let is_graph_empty = graph.is_empty();
		let g = g.map(|g| {
			if is_graph_empty {
				self.named.remove_entry(g).unwrap().0
			} else {
				self.named.get_key_value(g).unwrap().0.clone()
			}
		});

		Some(Quad(s, p, o, g))
	}

	pub fn take_match<
		T: ?Sized + Equivalent<S> + Hash,
		U: ?Sized + Equivalent<P> + Hash,
		V: ?Sized + Equivalent<O> + Hash,
		W: ?Sized + Equivalent<G> + Hash,
	>(
		&mut self,
		Quad(s, p, o, g): Quad<Option<&T>, Option<&U>, Option<&V>, Option<&W>>,
	) -> Option<Quad<S, P, O, G>>
	where
		S: Clone,
		P: Clone,
		O: Clone,
		G: Clone,
	{
		match g {
			Some(g) => {
				let graph = self.graph_mut(g)?;
				let Triple(s, p, o) = graph.take_match(Triple(s, p, o))?;

				let is_graph_empty = graph.is_empty();
				let g = g.map(|g| {
					if is_graph_empty {
						self.named.remove_entry(g).unwrap().0
					} else {
						self.named.get_key_value(g).unwrap().0.clone()
					}
				});

				Some(Quad(s, p, o, g))
			}
			None => {
				for (g, graph) in self.graphs_mut() {
					if let Some(Triple(s, p, o)) = graph.take_match(Triple(s, p, o)) {
						return Some(Quad(s, p, o, g.cloned()));
					}
				}

				None
			}
		}
	}

	pub fn absorb<D: crate::SizedDataset<Subject = S, Predicate = P, Object = O, GraphLabel = G>>(
		&mut self,
		other: D,
	) where
		D::Graph: crate::SizedGraph,
	{
		for (id, graph) in other.into_graphs() {
			match self.graph_mut(id.as_ref()) {
				Some(g) => g.absorb(graph),
				None => {
					self.insert_graph(id.unwrap(), HashGraph::from_graph(graph));
				}
			}
		}
	}

	/// Substitutes the blank node identifiers in the dataset.
	pub fn substitute_blank_ids<B>(self, f: impl Fn(B) -> B) -> Self
	where
		S: Clone + MaybeBlankId<BlankId = B> + IntoBlankId + FromBlankId,
		P: Clone + MaybeBlankId<BlankId = B> + IntoBlankId + FromBlankId,
		O: MaybeBlankId<BlankId = B> + IntoBlankId + FromBlankId,
		G: Clone + MaybeBlankId<BlankId = B> + IntoBlankId + FromBlankId,
	{
		let mut result = Self::new();

		fn substitute_term<T: IntoBlankId + FromBlankId>(
			term: T,
			f: impl Fn(T::BlankId) -> T::BlankId,
		) -> T {
			match term.try_into_blank() {
				Ok(b) => T::from_blank(f(b)),
				Err(term) => term,
			}
		}

		fn substitute_quad<B, S, P, O, G>(
			Quad(s, p, o, g): Quad<S, P, O, G>,
			f: impl Fn(B) -> B,
		) -> Quad<S, P, O, G>
		where
			S: MaybeBlankId<BlankId = B> + IntoBlankId + FromBlankId,
			P: MaybeBlankId<BlankId = B> + IntoBlankId + FromBlankId,
			O: MaybeBlankId<BlankId = B> + IntoBlankId + FromBlankId,
			G: MaybeBlankId<BlankId = B> + IntoBlankId + FromBlankId,
		{
			Quad(
				substitute_term(s, &f),
				substitute_term(p, &f),
				substitute_term(o, &f),
				g.map(|g| substitute_term(g, f)),
			)
		}

		for quad in self.into_quads() {
			result.insert(substitute_quad(quad, &f));
		}

		result
	}

	pub fn view<'a, A>(
		&'a self,
		graph_label: Option<&'a G>,
		subject: &'a S,
		access: A,
	) -> View<'a, Self, A> {
		crate::Dataset::view(self, graph_label, subject, access)
	}
}

impl<S: Eq + Hash, P: Eq + Hash, O: Eq + Hash, G: Eq + Hash> HashDataset<S, P, O, G> {
	/// Checks that there is an isomorphism between this dataset and `other`.
	///
	/// There is an isomorphism if there exists a blank node identifier bijection
	/// between `self` and `other`.
	/// This is equivalent to `self.find_blank_id_bijection(other).is_some()`.
	pub fn is_isomorphic_to<I, B, L>(&self, other: &Self) -> bool
	where
		S: AsRdfTerm<I, B, L>,
		P: AsRdfTerm<I, B, L>,
		O: AsRdfTerm<I, B, L>,
		G: AsRdfTerm<I, B, L>,
		I: PartialEq,
		L: PartialEq,
		B: Eq + Hash,
	{
		self.find_blank_id_bijection(other).is_some()
	}

	/// Finds a blank node identifier bijection between from `self` to `other`.
	/// If such bijection exists,
	/// there is an isomorphism between `self` and `other`.
	pub fn find_blank_id_bijection<'u, 'v, I: 'u + 'v, B, L: 'u + 'v>(
		&'u self,
		other: &'v Self,
	) -> Option<HashBijection<'u, 'v, B, B>>
	where
		S: AsRdfTerm<I, B, L>,
		P: AsRdfTerm<I, B, L>,
		O: AsRdfTerm<I, B, L>,
		G: AsRdfTerm<I, B, L>,
		I: PartialEq,
		L: PartialEq,
		B: Eq + Hash,
	{
		use crate::utils::isomorphism::hash::FindHashBlankIdBijection;

		fn has_no_blank<I, B, L, S, P, O, G>(Quad(s, p, o, g): &Quad<&S, &P, &O, &G>) -> bool
		where
			S: AsRdfTerm<I, B, L>,
			P: AsRdfTerm<I, B, L>,
			O: AsRdfTerm<I, B, L>,
			G: AsRdfTerm<I, B, L>,
		{
			!s.as_rdf_term().is_blank()
				&& !p.as_rdf_term().is_blank()
				&& !o.as_rdf_term().is_blank()
				&& !g.map(|g| g.as_rdf_term().is_blank()).unwrap_or(false)
		}

		let a_non_blank: HashSet<_> = self.quads().filter(has_no_blank).collect();
		let b_non_blank: HashSet<_> = other.quads().filter(has_no_blank).collect();

		if a_non_blank == b_non_blank {
			Self::find_hash_blank_id_bijection(self, other)
		} else {
			None
		}
	}
}

impl<S: Eq + Hash, P: Eq + Hash, O: Eq + Hash, G: Eq + Hash> crate::Dataset
	for HashDataset<S, P, O, G>
{
	type Subject = S;
	type Predicate = P;
	type Object = O;
	type GraphLabel = G;

	type Graph = HashGraph<S, P, O>;
	type Graphs<'a> = Graphs<'a, S, P, O, G> where
		Self: 'a,
		S: 'a,
		P: 'a,
		O: 'a,
		G: 'a;
	type Quads<'a> = Quads<'a, S, P, O, G> where
		Self: 'a,
		S: 'a,
		P: 'a,
		O: 'a;

	type PatternMatching<'a, 'p> = PatternMatching<'a, 'p, S, P, O, G> where Self: 'a,
		S: 'p,
		P: 'p,
		O: 'p,
		G: 'p;

	fn graph(&self, id: Option<&G>) -> Option<&HashGraph<S, P, O>> {
		self.graph(id)
	}

	fn graphs(&self) -> Graphs<'_, S, P, O, G> {
		self.graphs()
	}

	fn quads(&self) -> Quads<'_, S, P, O, G> {
		self.quads()
	}

	fn pattern_matching<'p>(
		&self,
		Quad(s, p, o, g): Quad<
			Option<&'p Self::Subject>,
			Option<&'p Self::Predicate>,
			Option<&'p Self::Object>,
			Option<&'p Self::GraphLabel>,
		>,
	) -> Self::PatternMatching<'_, 'p> {
		let pattern = Triple(s, p, o);

		match g {
			Some(g) => PatternMatching {
				pattern,
				graphs: None,
				current_graph: self
					.graph_entry(g)
					.map(|(g, graph)| (g, GraphPatternMatching::new(graph, pattern))),
			},
			None => PatternMatching {
				pattern,
				graphs: Some(self.graphs()),
				current_graph: None,
			},
		}
	}
}

impl<S: Eq + Hash, P: Eq + Hash, O: Eq + Hash, G: Eq + Hash> HashDataset<S, P, O, G> {
	crate::macros::reflect_dataset_impl!();
}

impl<S: fmt::Debug, P: fmt::Debug, O: fmt::Debug, G: fmt::Debug> fmt::Debug
	for HashDataset<S, P, O, G>
{
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{{")?;

		for (i, rdf_types::Quad(s, p, o, g)) in self.quads().enumerate() {
			if i > 0 {
				write!(f, ",")?;
			}

			match g {
				Some(g) => write!(f, " {s:?} {p:?} {o:?} {g:?}")?,
				None => write!(f, " {s:?} {p:?} {o:?}")?,
			}
		}

		write!(f, "  }}")
	}
}

#[allow(clippy::type_complexity)]
pub struct PatternMatching<'a, 'p, S, P, O, G> {
	pattern: Triple<Option<&'p S>, Option<&'p P>, Option<&'p O>>,
	graphs: Option<Graphs<'a, S, P, O, G>>,
	current_graph: Option<(Option<&'a G>, GraphPatternMatching<'a, 'p, S, P, O>)>,
}

impl<'a, 'p, S: Eq + Hash, P: Eq + Hash, O: Eq + Hash, G: Eq + Hash> Iterator
	for PatternMatching<'a, 'p, S, P, O, G>
{
	type Item = Quad<&'a S, &'a P, &'a O, &'a G>;

	fn next(&mut self) -> Option<Self::Item> {
		loop {
			match &mut self.current_graph {
				Some((g, m)) => match m.next() {
					Some(Triple(s, p, o)) => break Some(Quad(s, p, o, *g)),
					None => self.current_graph = None,
				},
				None => match &mut self.graphs {
					Some(graphs) => match graphs.next() {
						Some((g, graph)) => {
							self.current_graph =
								Some((g, GraphPatternMatching::new(graph, self.pattern)))
						}
						None => break None,
					},
					None => break None,
				},
			}
		}
	}
}

pub struct GraphPatternMatching<'a, 'p, S, P, O> {
	predicate_pattern: Option<&'p P>,
	object_pattern: Option<&'p O>,
	subjects: Option<hashbrown::hash_map::Iter<'a, S, HashMap<P, HashSet<O>>>>,
	current_subject: Option<(&'a S, SubjectPatternMatching<'a, 'p, P, O>)>,
}

impl<'a, 'p, S: Eq + Hash, P: Eq + Hash, O: Eq + Hash> GraphPatternMatching<'a, 'p, S, P, O> {
	fn new(
		graph: &'a HashGraph<S, P, O>,
		pattern: Triple<Option<&'p S>, Option<&'p P>, Option<&'p O>>,
	) -> Self {
		match pattern.into_subject() {
			Some(s) => Self {
				predicate_pattern: pattern.into_predicate(),
				object_pattern: pattern.into_object(),
				subjects: None,
				current_subject: graph.table.get_key_value(s).map(|(s, subject)| {
					(
						s,
						SubjectPatternMatching::new(
							subject,
							pattern.into_predicate(),
							pattern.into_object(),
						),
					)
				}),
			},
			None => Self {
				predicate_pattern: pattern.into_predicate(),
				object_pattern: pattern.into_object(),
				subjects: Some(graph.table.iter()),
				current_subject: None,
			},
		}
	}
}

impl<'a, 'p, S: Eq + Hash, P: Eq + Hash, O: Eq + Hash> Iterator
	for GraphPatternMatching<'a, 'p, S, P, O>
{
	type Item = Triple<&'a S, &'a P, &'a O>;

	fn next(&mut self) -> Option<Self::Item> {
		loop {
			match &mut self.current_subject {
				Some((s, m)) => match m.next() {
					Some((p, o)) => break Some(Triple(s, p, o)),
					None => self.current_subject = None,
				},
				None => match &mut self.subjects {
					Some(subjects) => match subjects.next() {
						Some((s, subject)) => {
							self.current_subject = Some((
								s,
								SubjectPatternMatching::new(
									subject,
									self.predicate_pattern,
									self.object_pattern,
								),
							))
						}
						None => break None,
					},
					None => break None,
				},
			}
		}
	}
}

struct SubjectPatternMatching<'a, 'p, P, O> {
	object_pattern: Option<&'p O>,
	predicates: Option<hashbrown::hash_map::Iter<'a, P, HashSet<O>>>,
	current_predicate: Option<(&'a P, PredicatePatternMatching<'a, O>)>,
}

impl<'a, 'p, P: Eq + Hash, O: Eq + Hash> SubjectPatternMatching<'a, 'p, P, O> {
	fn new(
		subject: &'a HashMap<P, HashSet<O>>,
		predicate_pattern: Option<&'p P>,
		object_pattern: Option<&'p O>,
	) -> Self {
		match predicate_pattern {
			Some(p) => Self {
				object_pattern,
				predicates: None,
				current_predicate: subject
					.get_key_value(p)
					.map(|(p, pred)| (p, PredicatePatternMatching::new(pred, object_pattern))),
			},
			None => Self {
				object_pattern,
				predicates: Some(subject.iter()),
				current_predicate: None,
			},
		}
	}
}

impl<'a, 'p, P: Eq + Hash, O: Eq + Hash> Iterator for SubjectPatternMatching<'a, 'p, P, O> {
	type Item = (&'a P, &'a O);

	fn next(&mut self) -> Option<Self::Item> {
		loop {
			match &mut self.current_predicate {
				Some((p, m)) => match m.next() {
					Some(o) => break Some((p, o)),
					None => self.current_predicate = None,
				},
				None => match &mut self.predicates {
					Some(predicates) => match predicates.next() {
						Some((p, pred)) => {
							self.current_predicate =
								Some((p, PredicatePatternMatching::new(pred, self.object_pattern)))
						}
						None => break None,
					},
					None => break None,
				},
			}
		}
	}
}

enum PredicatePatternMatching<'a, O> {
	One(Option<&'a O>),
	Any(hashbrown::hash_set::Iter<'a, O>),
}

impl<'a, O: Eq + Hash> PredicatePatternMatching<'a, O> {
	fn new(predicate: &'a HashSet<O>, object_pattern: Option<&O>) -> Self {
		match object_pattern {
			Some(o) => Self::One(predicate.get(o)),
			None => Self::Any(predicate.iter()),
		}
	}
}

impl<'a, O: Eq + Hash> Iterator for PredicatePatternMatching<'a, O> {
	type Item = &'a O;

	fn next(&mut self) -> Option<Self::Item> {
		match self {
			Self::One(o) => o.take(),
			Self::Any(iter) => iter.next(),
		}
	}
}

#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
pub struct Graphs<'a, S, P, O, G> {
	default: Option<&'a HashGraph<S, P, O>>,
	it: hashbrown::hash_map::Iter<'a, G, HashGraph<S, P, O>>,
}

impl<'a, S, P, O, G> Iterator for Graphs<'a, S, P, O, G> {
	type Item = (Option<&'a G>, &'a HashGraph<S, P, O>);

	fn next(&mut self) -> Option<Self::Item> {
		if let Some(default) = self.default {
			self.default = None;
			Some((None, default))
		} else {
			self.it.next().map(|(id, graph)| (Some(id), graph))
		}
	}
}

pub struct GraphsMut<'a, S, P, O, G> {
	default: Option<&'a mut HashGraph<S, P, O>>,
	it: hashbrown::hash_map::IterMut<'a, G, HashGraph<S, P, O>>,
}

impl<'a, S, P, O, G> Iterator for GraphsMut<'a, S, P, O, G> {
	type Item = (Option<&'a G>, &'a mut HashGraph<S, P, O>);

	fn next(&mut self) -> Option<Self::Item> {
		let mut default = None;
		std::mem::swap(&mut default, &mut self.default);
		if let Some(default) = default {
			self.default = None;
			Some((None, default))
		} else {
			self.it.next().map(|(id, graph)| (Some(id), graph))
		}
	}
}

#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
pub struct Quads<'a, S, P, O, G> {
	graphs: Graphs<'a, S, P, O, G>,
	graph: Option<&'a G>,
	triples: Option<Iter<'a, S, P, O>>,
}

impl<'a, S, P, O, G> Iterator for Quads<'a, S, P, O, G> {
	type Item = Quad<&'a S, &'a P, &'a O, &'a G>;

	fn next(&mut self) -> Option<Quad<&'a S, &'a P, &'a O, &'a G>> {
		loop {
			match &mut self.triples {
				Some(triples) => match triples.next() {
					Some(triple) => return Some(Quad(triple.0, triple.1, triple.2, self.graph)),
					None => {
						self.triples = None;
					}
				},
				None => match self.graphs.next() {
					Some((id, graph)) => {
						self.graph = id;
						self.triples = Some(graph.triples())
					}
					None => return None,
				},
			}
		}
	}
}

impl<S: Clone + Eq + Hash, P: Clone + Eq + Hash, O: Eq + Hash, G: Clone + Eq + Hash>
	crate::SizedDataset for HashDataset<S, P, O, G>
{
	type IntoGraphs = IntoGraphs<S, P, O, G>;
	type IntoQuads = IntoQuads<S, P, O, G>;

	fn into_graph(self, id: Option<&G>) -> Option<Self::Graph> {
		self.into_graph(id)
	}

	fn into_graphs(self) -> Self::IntoGraphs {
		self.into_graphs()
	}

	fn into_quads(self) -> Self::IntoQuads {
		self.into_quads()
	}
}

impl<S: Clone + Eq + Hash, P: Clone + Eq + Hash, O: Eq + Hash, G: Clone + Eq + Hash>
	HashDataset<S, P, O, G>
{
	crate::macros::reflect_sized_dataset_impl!();
}

pub struct IntoGraphs<S, P, O, G> {
	default: Option<HashGraph<S, P, O>>,
	it: hashbrown::hash_map::IntoIter<G, HashGraph<S, P, O>>,
}

impl<S, P, O, G> Iterator for IntoGraphs<S, P, O, G> {
	type Item = (Option<G>, HashGraph<S, P, O>);

	fn next(&mut self) -> Option<Self::Item> {
		let mut default = None;
		std::mem::swap(&mut default, &mut self.default);
		if let Some(default) = default {
			self.default = None;
			Some((None, default))
		} else {
			self.it.next().map(|(id, graph)| (Some(id), graph))
		}
	}
}

pub struct IntoQuads<S = rdf_types::Term, P = S, O = S, G = S> {
	graphs: IntoGraphs<S, P, O, G>,
	graph: Option<G>,
	triples: Option<IntoIter<S, P, O>>,
}

impl<S: Clone, P: Clone, O, G: Clone> Iterator for IntoQuads<S, P, O, G> {
	type Item = Quad<S, P, O, G>;

	fn next(&mut self) -> Option<Quad<S, P, O, G>> {
		loop {
			match &mut self.triples {
				Some(triples) => match triples.next() {
					Some(triple) => {
						return Some(Quad(triple.0, triple.1, triple.2, self.graph.clone()))
					}
					None => {
						self.triples = None;
					}
				},
				None => match self.graphs.next() {
					Some((id, graph)) => {
						self.graph = id;
						self.triples = Some(graph.into_triples())
					}
					None => return None,
				},
			}
		}
	}
}

impl<S: Eq + Hash, P: Eq + Hash, O: Eq + Hash, G: Eq + Hash> crate::MutableDataset
	for HashDataset<S, P, O, G>
{
	type GraphsMut<'a> = GraphsMut<'a, S, P, O, G> where
		Self: 'a,
		S: 'a,
		P: 'a,
		O: 'a,
		G: 'a;

	fn graph_mut(&mut self, id: Option<&G>) -> Option<&mut Self::Graph> {
		self.graph_mut(id)
	}

	fn graphs_mut(&mut self) -> Self::GraphsMut<'_> {
		self.graphs_mut()
	}

	fn insert_graph(&mut self, id: G, graph: Self::Graph) -> Option<Self::Graph> {
		self.named.insert(id, graph)
	}

	fn insert(&mut self, quad: Quad<S, P, O, G>) -> bool {
		self.insert(quad)
	}

	fn remove(&mut self, quad: Quad<&S, &P, &O, &G>) {
		self.remove(quad)
	}

	fn absorb<D: crate::SizedDataset<Subject = S, Predicate = P, Object = O, GraphLabel = G>>(
		&mut self,
		other: D,
	) where
		D::Graph: crate::SizedGraph,
	{
		self.absorb(other)
	}
}

impl<S: Eq + Hash, P: Eq + Hash, O: Eq + Hash, G: Eq + Hash> HashDataset<S, P, O, G> {
	crate::macros::reflect_mutable_dataset_impl!();
}

impl<'a, S, P, O, G> IntoIterator for &'a HashDataset<S, P, O, G> {
	type Item = Quad<&'a S, &'a P, &'a O, &'a G>;
	type IntoIter = Quads<'a, S, P, O, G>;

	fn into_iter(self) -> Self::IntoIter {
		self.quads()
	}
}

impl<S: Clone, P: Clone, O, G: Clone> IntoIterator for HashDataset<S, P, O, G> {
	type Item = Quad<S, P, O, G>;
	type IntoIter = IntoQuads<S, P, O, G>;

	fn into_iter(self) -> Self::IntoIter {
		self.into_quads()
	}
}

impl<S: Eq + Hash, P: Eq + Hash, O: Eq + Hash, G: Eq + Hash>
	std::iter::FromIterator<Quad<S, P, O, G>> for HashDataset<S, P, O, G>
{
	fn from_iter<I: IntoIterator<Item = Quad<S, P, O, G>>>(iter: I) -> Self {
		let mut ds = Self::new();
		ds.extend(iter);
		ds
	}
}

impl<S: Eq + Hash, P: Eq + Hash, O: Eq + Hash, G: Eq + Hash> std::iter::Extend<Quad<S, P, O, G>>
	for HashDataset<S, P, O, G>
{
	fn extend<I: IntoIterator<Item = Quad<S, P, O, G>>>(&mut self, iter: I) {
		for quad in iter {
			self.insert(quad);
		}
	}
}
