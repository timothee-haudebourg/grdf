//! Dataset implementation based on `HashMap` and `HashSet`.
use crate::{utils::HashBijection, Quad, Triple};
use derivative::Derivative;
use rdf_types::{AsTerm, IntoTerm};
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::Hash;

/// Graph implementation based on `HashMap` and `HashSet`.
#[derive(Derivative)]
#[derivative(PartialEq(bound = "S: Eq + Hash, P: Eq + Hash, O: Eq + Hash"))]
#[derivative(Eq(bound = "S: Eq + Hash, P: Eq + Hash, O: Eq + Hash"))]
#[derivative(Default(bound = ""))]
pub struct HashGraph<S = rdf_types::Term, P = S, O = S> {
	table: HashMap<S, HashMap<P, HashSet<O>>>,
}

impl<S, P, O> HashGraph<S, P, O> {
	/// Create a new empty `HashGraph`.
	pub fn new() -> Self {
		Self::default()
	}
}

impl<S: Eq + Hash, P: Eq + Hash, O: Eq + Hash> HashGraph<S, P, O> {
	/// Create a new `HashGraph` from another graph by consuming its triples.
	pub fn from_graph<G: crate::SizedGraph<Subject = S, Predicate = P, Object = O>>(
		g: G,
	) -> HashGraph<S, P, O> {
		let mut subject_map = HashMap::new();
		for (subject, predicates) in g.into_subjects() {
			let mut predicate_map = HashMap::new();
			for (predicate, objects) in predicates {
				predicate_map.insert(predicate, objects.collect());
			}

			subject_map.insert(subject, predicate_map);
		}

		HashGraph { table: subject_map }
	}

	pub fn insert(&mut self, triple: Triple<S, P, O>) {
		let (subject, predicate, object) = triple.into_parts();

		match self.table.get_mut(&subject) {
			Some(bindings) => match bindings.get_mut(&predicate) {
				Some(objects) => {
					objects.insert(object);
				}
				None => {
					let mut objects = HashSet::new();
					objects.insert(object);
					bindings.insert(predicate, objects);
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

	pub fn absorb<G: crate::SizedGraph<Subject = S, Predicate = P, Object = O>>(
		&mut self,
		other: G,
	) {
		let subjects = other.into_subjects();

		for (subject, predicates) in subjects {
			match self.table.get_mut(&subject) {
				Some(bindings) => {
					for (predicate, objects) in predicates {
						match bindings.get_mut(&predicate) {
							Some(other_objects) => {
								other_objects.extend(objects);
							}
							None => {
								bindings.insert(predicate, objects.collect());
							}
						}
					}
				}
				None => {
					let mut bindings = HashMap::new();
					for (predicate, objects) in predicates {
						bindings.insert(predicate, objects.collect());
					}

					self.table.insert(subject, bindings);
				}
			}
		}
	}
}

impl<S, P, O> HashGraph<S, P, O> {
	pub fn triples(&self) -> Iter<S, P, O> {
		Iter {
			subjects: self.subjects(),
			subject: None,
			predicates: None,
			predicate: None,
			objects: None,
		}
	}

	pub fn subjects(&self) -> Subjects<S, P, O> {
		Subjects {
			it: self.table.iter(),
		}
	}

	pub fn predicates(&self, subject: &S) -> Predicates<P, O>
	where
		S: Eq + Hash,
	{
		match self.table.get(subject) {
			Some(map) => Predicates {
				it: Some(map.iter()),
			},
			None => Predicates { it: None },
		}
	}

	pub fn objects(&self, subject: &S, predicate: &P) -> Objects<O>
	where
		S: Eq + Hash,
		P: Eq + Hash,
	{
		match self.table.get(subject) {
			Some(map) => match map.get(predicate) {
				Some(map) => Objects {
					it: Some(map.iter()),
				},
				None => Objects { it: None },
			},
			None => Objects { it: None },
		}
	}

	pub fn contains(&self, Triple(subject, predicate, object): Triple<&S, &P, &O>) -> bool
	where
		S: Eq + Hash,
		P: Eq + Hash,
		O: Eq + Hash,
	{
		match self.table.get(subject) {
			Some(map) => match map.get(predicate) {
				Some(map) => map.contains(object),
				None => false,
			},
			None => false,
		}
	}

	pub fn into_triples(self) -> IntoIter<S, P, O> {
		IntoIter {
			subjects: self.into_subjects(),
			subject: None,
			predicates: None,
			predicate: None,
			objects: None,
		}
	}

	pub fn into_subjects(self) -> IntoSubjects<S, P, O> {
		IntoSubjects {
			it: self.table.into_iter(),
		}
	}

	pub fn into_predicates(mut self, subject: &S) -> IntoPredicates<P, O>
	where
		S: Eq + Hash,
	{
		match self.table.remove(subject) {
			Some(map) => IntoPredicates {
				it: Some(map.into_iter()),
			},
			None => IntoPredicates { it: None },
		}
	}

	pub fn into_objects(mut self, subject: &S, predicate: &P) -> IntoObjects<O>
	where
		S: Eq + Hash,
		P: Eq + Hash,
	{
		match self.table.remove(subject) {
			Some(mut map) => match map.remove(predicate) {
				Some(map) => IntoObjects {
					it: Some(map.into_iter()),
				},
				None => IntoObjects { it: None },
			},
			None => IntoObjects { it: None },
		}
	}
}

impl<S: Eq + Hash, P: Eq + Hash, O: Eq + Hash> crate::Graph for HashGraph<S, P, O> {
	type Subject = S;
	type Predicate = P;
	type Object = O;

	type Objects<'a> = Objects<'a, O> where
		Self: 'a,
		O: 'a;
	type Predicates<'a> = Predicates<'a, P, O> where
		Self: 'a,
		P: 'a,
		O: 'a;
	type Subjects<'a> = Subjects<'a, S, P, O> where
		Self: 'a,
		S: 'a,
		P: 'a,
		O: 'a;
	type Triples<'a> = Iter<'a, S, P, O> where
		Self: 'a,
		S: 'a,
		P: 'a,
		O: 'a;

	fn triples<'a>(&'a self) -> Iter<'a, S, P, O>
	where
		S: 'a,
		P: 'a,
		O: 'a,
	{
		self.triples()
	}

	fn subjects<'a>(&'a self) -> Subjects<'a, S, P, O>
	where
		S: 'a,
		P: 'a,
		O: 'a,
	{
		self.subjects()
	}

	fn predicates<'a>(&'a self, subject: &S) -> Predicates<'a, P, O>
	where
		P: 'a,
		O: 'a,
	{
		self.predicates(subject)
	}

	fn objects<'a>(&'a self, subject: &S, predicate: &P) -> Objects<'a, O>
	where
		O: 'a,
	{
		self.objects(subject, predicate)
	}

	fn contains(&self, triple: Triple<&S, &P, &O>) -> bool {
		self.contains(triple)
	}
}

impl<'a, S, P, O> std::iter::IntoIterator for &'a HashGraph<S, P, O> {
	type IntoIter = Iter<'a, S, P, O>;
	type Item = Triple<&'a S, &'a P, &'a O>;

	fn into_iter(self) -> Self::IntoIter {
		self.triples()
	}
}

impl<S: Clone + Eq + Hash, P: Clone + Eq + Hash, O: Eq + Hash> crate::SizedGraph
	for HashGraph<S, P, O>
{
	type IntoObjects = IntoObjects<O>;
	type IntoPredicates = IntoPredicates<P, O>;
	type IntoSubjects = IntoSubjects<S, P, O>;
	type IntoTriples = IntoIter<S, P, O>;

	fn into_triples(self) -> IntoIter<S, P, O> {
		self.into_triples()
	}

	fn into_subjects(self) -> IntoSubjects<S, P, O> {
		self.into_subjects()
	}

	fn into_predicates(self, subject: &S) -> IntoPredicates<P, O> {
		self.into_predicates(subject)
	}

	fn into_objects(self, subject: &S, predicate: &P) -> IntoObjects<O> {
		self.into_objects(subject, predicate)
	}
}

impl<S: Clone + Eq + Hash, P: Clone + Eq + Hash, O: Eq + Hash> std::iter::IntoIterator
	for HashGraph<S, P, O>
{
	type IntoIter = IntoIter<S, P, O>;
	type Item = Triple<S, P, O>;

	fn into_iter(self) -> Self::IntoIter {
		self.into_triples()
	}
}

impl<S: Eq + Hash, P: Eq + Hash, O: Eq + Hash> crate::MutableGraph for HashGraph<S, P, O> {
	fn insert(&mut self, triple: Triple<S, P, O>) {
		self.insert(triple)
	}

	fn absorb<G: crate::SizedGraph<Subject = S, Predicate = P, Object = O>>(&mut self, other: G) {
		self.absorb(other)
	}
}

impl<S: fmt::Debug, P: fmt::Debug, O: fmt::Debug> fmt::Debug for HashGraph<S, P, O> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{{")?;

		for (i, rdf_types::Triple(s, p, o)) in self.triples().enumerate() {
			if i > 0 {
				write!(f, ",")?;
			}

			write!(f, " {:?} {:?} {:?}", s, p, o)?;
		}

		write!(f, "  }}")
	}
}

pub struct Subjects<'a, S, P, O> {
	it: std::collections::hash_map::Iter<'a, S, HashMap<P, HashSet<O>>>,
}

impl<'a, S, P, O> Iterator for Subjects<'a, S, P, O> {
	type Item = (&'a S, Predicates<'a, P, O>);

	fn next(&mut self) -> Option<Self::Item> {
		self.it.next().map(|(subject, map)| {
			(
				subject,
				Predicates {
					it: Some(map.iter()),
				},
			)
		})
	}
}

pub struct IntoSubjects<S, P, O> {
	it: std::collections::hash_map::IntoIter<S, HashMap<P, HashSet<O>>>,
}

impl<S, P, O> Iterator for IntoSubjects<S, P, O> {
	type Item = (S, IntoPredicates<P, O>);

	fn next(&mut self) -> Option<Self::Item> {
		self.it.next().map(|(subject, map)| {
			(
				subject,
				IntoPredicates {
					it: Some(map.into_iter()),
				},
			)
		})
	}
}

pub struct Predicates<'a, P, O> {
	it: Option<std::collections::hash_map::Iter<'a, P, HashSet<O>>>,
}

impl<'a, P, O> Iterator for Predicates<'a, P, O> {
	type Item = (&'a P, Objects<'a, O>);

	fn next(&mut self) -> Option<Self::Item> {
		match &mut self.it {
			Some(it) => it.next().map(|(predicate, set)| {
				(
					predicate,
					Objects {
						it: Some(set.iter()),
					},
				)
			}),
			None => None,
		}
	}
}

pub struct IntoPredicates<P, O> {
	it: Option<std::collections::hash_map::IntoIter<P, HashSet<O>>>,
}

impl<P, O> Iterator for IntoPredicates<P, O> {
	type Item = (P, IntoObjects<O>);

	fn next(&mut self) -> Option<Self::Item> {
		match &mut self.it {
			Some(it) => it.next().map(|(predicate, set)| {
				(
					predicate,
					IntoObjects {
						it: Some(set.into_iter()),
					},
				)
			}),
			None => None,
		}
	}
}

pub struct Objects<'a, O> {
	it: Option<std::collections::hash_set::Iter<'a, O>>,
}

impl<'a, O> Iterator for Objects<'a, O> {
	type Item = &'a O;

	fn next(&mut self) -> Option<Self::Item> {
		match &mut self.it {
			Some(it) => it.next(),
			None => None,
		}
	}
}

pub struct IntoObjects<O> {
	it: Option<std::collections::hash_set::IntoIter<O>>,
}

impl<O> Iterator for IntoObjects<O> {
	type Item = O;

	fn next(&mut self) -> Option<Self::Item> {
		match &mut self.it {
			Some(it) => it.next(),
			None => None,
		}
	}
}

pub struct Iter<'a, S, P, O> {
	subjects: Subjects<'a, S, P, O>,
	subject: Option<&'a S>,
	predicates: Option<Predicates<'a, P, O>>,
	predicate: Option<&'a P>,
	objects: Option<Objects<'a, O>>,
}

impl<'a, S, P, O> Iterator for Iter<'a, S, P, O> {
	type Item = Triple<&'a S, &'a P, &'a O>;

	fn next(&mut self) -> Option<Self::Item> {
		loop {
			match &mut self.objects {
				Some(objects) => match objects.next() {
					Some(object) => {
						return Some(Triple(
							self.subject.unwrap(),
							self.predicate.unwrap(),
							object,
						))
					}
					None => {
						self.objects = None;
					}
				},
				None => match &mut self.predicates {
					Some(predicates) => match predicates.next() {
						Some((predicate, objects)) => {
							self.predicate = Some(predicate);
							self.objects = Some(objects)
						}
						None => {
							self.predicates = None;
							self.predicate = None;
						}
					},
					None => match self.subjects.next() {
						Some((subject, predicates)) => {
							self.subject = Some(subject);
							self.predicates = Some(predicates)
						}
						None => return None,
					},
				},
			}
		}
	}
}

pub struct IntoIter<S, P, O> {
	subjects: IntoSubjects<S, P, O>,
	subject: Option<S>,
	predicates: Option<IntoPredicates<P, O>>,
	predicate: Option<P>,
	objects: Option<IntoObjects<O>>,
}

impl<S: Clone, P: Clone, O> Iterator for IntoIter<S, P, O> {
	type Item = Triple<S, P, O>;

	fn next(&mut self) -> Option<Self::Item> {
		loop {
			match &mut self.objects {
				Some(objects) => match objects.next() {
					Some(object) => {
						return Some(Triple(
							self.subject.clone().unwrap(),
							self.predicate.clone().unwrap(),
							object,
						))
					}
					None => {
						self.objects = None;
					}
				},
				None => match &mut self.predicates {
					Some(predicates) => match predicates.next() {
						Some((predicate, objects)) => {
							self.predicate = Some(predicate);
							self.objects = Some(objects)
						}
						None => {
							self.predicates = None;
							self.predicate = None;
						}
					},
					None => match self.subjects.next() {
						Some((subject, predicates)) => {
							self.subject = Some(subject);
							self.predicates = Some(predicates)
						}
						None => return None,
					},
				},
			}
		}
	}
}

impl<S: Eq + Hash, P: Eq + Hash, O: Eq + Hash> std::iter::FromIterator<Triple<S, P, O>>
	for HashGraph<S, P, O>
{
	fn from_iter<I: IntoIterator<Item = Triple<S, P, O>>>(iter: I) -> Self {
		let mut ds = Self::new();
		ds.extend(iter);
		ds
	}
}

impl<S: Eq + Hash, P: Eq + Hash, O: Eq + Hash> std::iter::Extend<Triple<S, P, O>>
	for HashGraph<S, P, O>
{
	fn extend<I: IntoIterator<Item = Triple<S, P, O>>>(&mut self, iter: I) {
		for quad in iter {
			self.insert(quad);
		}
	}
}

#[derive(Derivative)]
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

	pub fn graph(&self, id: Option<&G>) -> Option<&HashGraph<S, P, O>>
	where
		G: Eq + Hash,
	{
		match id {
			Some(id) => self.named.get(id),
			None => Some(&self.default),
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

	pub fn graph_mut(&mut self, id: Option<&G>) -> Option<&mut HashGraph<S, P, O>>
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
	pub fn insert(&mut self, quad: Quad<S, P, O, G>) {
		let (subject, predicate, object, graph_name) = quad.into_parts();
		match self.graph_mut(graph_name.as_ref()) {
			Some(g) => g.insert(Triple(subject, predicate, object)),
			None => {
				let mut g = HashGraph::new();
				g.insert(Triple(subject, predicate, object));
				self.insert_graph(graph_name.unwrap(), g);
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
	pub fn substitute_blank_ids(self, f: impl Clone + Fn(S::BlankId) -> S::BlankId) -> Self
	where
		S: Clone + IntoTerm + From<rdf_types::Term<S::Iri, S::BlankId, S::Literal>>,
		P: Clone
			+ IntoTerm<BlankId = S::BlankId>
			+ From<rdf_types::Term<P::Iri, P::BlankId, P::Literal>>,
		O: IntoTerm<BlankId = S::BlankId> + From<rdf_types::Term<O::Iri, O::BlankId, O::Literal>>,
		G: Clone
			+ IntoTerm<BlankId = S::BlankId>
			+ From<rdf_types::Term<G::Iri, G::BlankId, G::Literal>>,
	{
		let mut result = Self::new();

		fn substitute_term<T: IntoTerm + From<rdf_types::Term<T::Iri, T::BlankId, T::Literal>>>(
			term: T,
			f: impl Clone + Fn(T::BlankId) -> T::BlankId,
		) -> T {
			match term.into_term() {
				rdf_types::Term::Blank(id) => rdf_types::Term::Blank(f(id)).into(),
				other => other.into(),
			}
		}

		fn substitute_quad<S, P, O, G>(
			Quad(s, p, o, g): Quad<S, P, O, G>,
			f: impl Clone + Fn(S::BlankId) -> S::BlankId,
		) -> Quad<S, P, O, G>
		where
			S: IntoTerm + From<rdf_types::Term<S::Iri, S::BlankId, S::Literal>>,
			P: IntoTerm<BlankId = S::BlankId>
				+ From<rdf_types::Term<P::Iri, P::BlankId, P::Literal>>,
			O: IntoTerm<BlankId = S::BlankId>
				+ From<rdf_types::Term<O::Iri, O::BlankId, O::Literal>>,
			G: IntoTerm<BlankId = S::BlankId>
				+ From<rdf_types::Term<G::Iri, G::BlankId, G::Literal>>,
		{
			Quad(
				substitute_term(s, f.clone()),
				substitute_term(p, f.clone()),
				substitute_term(o, f.clone()),
				g.map(|g| substitute_term(g, f)),
			)
		}

		for quad in self.into_quads() {
			result.insert(substitute_quad(quad, f.clone()))
		}

		result
	}
}

impl<S: Eq + Hash, P: Eq + Hash, O: Eq + Hash, G: Eq + Hash> HashDataset<S, P, O, G>
where
	S: AsTerm,
	<S as AsTerm>::Iri: PartialEq,
	<S as AsTerm>::Literal: PartialEq,
	<S as AsTerm>::BlankId: Eq + Hash,
	P: AsTerm<BlankId = <S as AsTerm>::BlankId>,
	<P as AsTerm>::Iri: PartialEq,
	<P as AsTerm>::Literal: PartialEq,
	O: AsTerm<BlankId = <S as AsTerm>::BlankId>,
	<O as AsTerm>::Iri: PartialEq,
	<O as AsTerm>::Literal: PartialEq,
	G: AsTerm<BlankId = <S as AsTerm>::BlankId>,
	<G as AsTerm>::Iri: PartialEq,
	<G as AsTerm>::Literal: PartialEq,
{
	/// Checks that there is an isomorphism between this dataset and `other`.
	///
	/// There is an isomorphism if there exists a blank node identifier bijection
	/// between `self` and `other`.
	/// This is equivalent to `self.find_blank_id_bijection(other).is_some()`.
	pub fn is_isomorphic_to(&self, other: &Self) -> bool {
		self.find_blank_id_bijection(other).is_some()
	}

	/// Finds a blank node identifier bijection between from `self` to `other`.
	/// If such bijection exists,
	/// there is an isomorphism between `self` and `other`.
	pub fn find_blank_id_bijection<'a, 'b>(
		&'a self,
		other: &'b Self,
	) -> Option<HashBijection<'a, 'b, <S as AsTerm>::BlankId, <S as AsTerm>::BlankId>> {
		use crate::utils::isomorphism::hash::FindHashBlankIdBijection;

		fn has_no_blank<S: AsTerm, P: AsTerm, O: AsTerm, G: AsTerm>(
			Quad(s, p, o, g): &Quad<&S, &P, &O, &G>,
		) -> bool {
			!s.as_term().is_blank()
				&& !p.as_term().is_blank()
				&& !o.as_term().is_blank()
				&& !g.map(|g| g.as_term().is_blank()).unwrap_or(false)
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

	fn graph(&self, id: Option<&G>) -> Option<&HashGraph<S, P, O>> {
		self.graph(id)
	}

	fn graphs(&self) -> Graphs<'_, S, P, O, G> {
		self.graphs()
	}

	fn quads(&self) -> Quads<'_, S, P, O, G> {
		self.quads()
	}
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
				Some(g) => write!(f, " {:?} {:?} {:?} {:?}", s, p, o, g)?,
				None => write!(f, " {:?} {:?} {:?}", s, p, o)?,
			}
		}

		write!(f, "  }}")
	}
}

pub struct Graphs<'a, S, P, O, G> {
	default: Option<&'a HashGraph<S, P, O>>,
	it: std::collections::hash_map::Iter<'a, G, HashGraph<S, P, O>>,
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
	it: std::collections::hash_map::IterMut<'a, G, HashGraph<S, P, O>>,
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

pub struct IntoGraphs<S, P, O, G> {
	default: Option<HashGraph<S, P, O>>,
	it: std::collections::hash_map::IntoIter<G, HashGraph<S, P, O>>,
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

	fn insert(&mut self, quad: Quad<S, P, O, G>) {
		self.insert(quad)
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
