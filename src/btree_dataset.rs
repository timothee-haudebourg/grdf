//! Dataset implementation based on `BTreeMap` and `BTreeSet`.
use crate::{utils::BTreeBijection, Quad, Triple};
use derivative::Derivative;
use rdf_types::{AsTerm, IntoTerm};
use std::collections::{BTreeMap, BTreeSet};
use std::fmt;
use std::hash::Hash;

/// Graph implementation based on `BTreeMap` and `BTreeSet`.
#[derive(Derivative)]
#[derivative(PartialEq(bound = "S: Ord, P: Ord, O: Ord"))]
#[derivative(Eq(bound = "S: Ord, P: Ord, O: Ord"))]
#[derivative(PartialOrd(bound = "S: Ord, P: Ord, O: Ord"))]
#[derivative(Ord(bound = "S: Ord, P: Ord, O: Ord"))]
#[derivative(Hash(bound = "S: Ord + Hash, P: Ord + Hash, O: Ord + Hash"))]
#[derivative(Default(bound = ""))]
pub struct BTreeGraph<S = rdf_types::Term, P = S, O = S> {
	table: BTreeMap<S, BTreeMap<P, BTreeSet<O>>>,
}

impl<S, P, O> BTreeGraph<S, P, O> {
	/// Create a new empty `BTreeGraph`.
	pub fn new() -> Self {
		Self::default()
	}
}

impl<S: fmt::Debug, P: fmt::Debug, O: fmt::Debug> fmt::Debug for BTreeGraph<S, P, O> {
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

impl<S: Ord, P: Ord, O: Ord> BTreeGraph<S, P, O> {
	/// Create a new `BTreeGraph` from another graph by consuming its triples.
	pub fn from_graph<G: crate::SizedGraph<Subject = S, Predicate = P, Object = O>>(
		g: G,
	) -> BTreeGraph<S, P, O> {
		let mut subject_map = BTreeMap::new();
		for (subject, predicates) in g.into_subjects() {
			let mut predicate_map = BTreeMap::new();
			for (predicate, objects) in predicates {
				predicate_map.insert(predicate, objects.collect());
			}

			subject_map.insert(subject, predicate_map);
		}

		BTreeGraph { table: subject_map }
	}

	pub fn insert(&mut self, triple: Triple<S, P, O>) {
		let (subject, predicate, object) = triple.into_parts();

		match self.table.get_mut(&subject) {
			Some(bindings) => match bindings.get_mut(&predicate) {
				Some(objects) => {
					objects.insert(object);
				}
				None => {
					let mut objects = BTreeSet::new();
					objects.insert(object);
					bindings.insert(predicate, objects);
				}
			},
			None => {
				let mut bindings = BTreeMap::new();
				let mut objects = BTreeSet::new();
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
					let mut bindings = BTreeMap::new();
					for (predicate, objects) in predicates {
						bindings.insert(predicate, objects.collect());
					}

					self.table.insert(subject, bindings);
				}
			}
		}
	}
}

impl<S, P, O> BTreeGraph<S, P, O> {
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
		S: Ord,
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
		S: Ord,
		P: Ord,
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
		S: Ord,
		P: Ord,
		O: Ord,
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
		S: Ord,
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
		S: Ord,
		P: Ord,
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

impl<S: Ord, P: Ord, O: Ord> crate::Graph for BTreeGraph<S, P, O> {
	type Subject = S;
	type Predicate = P;
	type Object = O;

	type Objects<'a>
	where
		Self: 'a,
		O: 'a,
	= Objects<'a, O>;
	type Predicates<'a>
	where
		Self: 'a,
		P: 'a,
		O: 'a,
	= Predicates<'a, P, O>;
	type Subjects<'a>
	where
		Self: 'a,
		S: 'a,
		P: 'a,
		O: 'a,
	= Subjects<'a, S, P, O>;
	type Triples<'a>
	where
		Self: 'a,
		S: 'a,
		P: 'a,
		O: 'a,
	= Iter<'a, S, P, O>;

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

impl<'a, S, P, O> std::iter::IntoIterator for &'a BTreeGraph<S, P, O> {
	type IntoIter = Iter<'a, S, P, O>;
	type Item = Triple<&'a S, &'a P, &'a O>;

	fn into_iter(self) -> Self::IntoIter {
		self.triples()
	}
}

impl<S: Clone + Ord, P: Clone + Ord, O: Ord> crate::SizedGraph for BTreeGraph<S, P, O> {
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

impl<S: Clone + Ord, P: Clone + Ord, O: Ord> std::iter::IntoIterator for BTreeGraph<S, P, O> {
	type IntoIter = IntoIter<S, P, O>;
	type Item = Triple<S, P, O>;

	fn into_iter(self) -> Self::IntoIter {
		self.into_triples()
	}
}

impl<S: Ord, P: Ord, O: Ord> crate::MutableGraph for BTreeGraph<S, P, O> {
	fn insert(&mut self, triple: Triple<S, P, O>) {
		self.insert(triple)
	}

	fn absorb<G: crate::SizedGraph<Subject = S, Predicate = P, Object = O>>(&mut self, other: G) {
		self.absorb(other)
	}
}

pub struct Subjects<'a, S, P, O> {
	it: std::collections::btree_map::Iter<'a, S, BTreeMap<P, BTreeSet<O>>>,
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
	it: std::collections::btree_map::IntoIter<S, BTreeMap<P, BTreeSet<O>>>,
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
	it: Option<std::collections::btree_map::Iter<'a, P, BTreeSet<O>>>,
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
	it: Option<std::collections::btree_map::IntoIter<P, BTreeSet<O>>>,
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
	it: Option<std::collections::btree_set::Iter<'a, O>>,
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
	it: Option<std::collections::btree_set::IntoIter<O>>,
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

impl<S: Ord, P: Ord, O: Ord> std::iter::FromIterator<Triple<S, P, O>> for BTreeGraph<S, P, O> {
	fn from_iter<I: IntoIterator<Item = Triple<S, P, O>>>(iter: I) -> Self {
		let mut ds = Self::new();
		ds.extend(iter);
		ds
	}
}

impl<S: Ord, P: Ord, O: Ord> std::iter::Extend<Triple<S, P, O>> for BTreeGraph<S, P, O> {
	fn extend<I: IntoIterator<Item = Triple<S, P, O>>>(&mut self, iter: I) {
		for triple in iter {
			self.insert(triple);
		}
	}
}

#[derive(Derivative)]
#[derivative(PartialEq(bound = "S: Ord, P: Ord, O: Ord, G: Ord"))]
#[derivative(Eq(bound = "S: Ord, P: Ord, O: Ord, G: Ord"))]
#[derivative(PartialOrd(bound = "S: Ord, P: Ord, O: Ord, G: Ord"))]
#[derivative(Ord(bound = "S: Ord, P: Ord, O: Ord, G: Ord"))]
#[derivative(Hash(bound = "S: Ord + Hash, P: Ord + Hash, O: Ord + Hash, G: Ord + Hash"))]
#[derivative(Default(bound = ""))]
pub struct BTreeDataset<S = rdf_types::Term, P = S, O = S, G = S> {
	default: BTreeGraph<S, P, O>,
	named: BTreeMap<G, BTreeGraph<S, P, O>>,
}

impl<S, P, O, G> BTreeDataset<S, P, O, G> {
	pub fn new() -> Self {
		Self::default()
	}

	pub fn graph(&self, id: Option<&G>) -> Option<&BTreeGraph<S, P, O>>
	where
		G: Ord,
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

	pub fn graph_mut(&mut self, id: Option<&G>) -> Option<&mut BTreeGraph<S, P, O>>
	where
		G: Ord,
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

	pub fn insert_graph(&mut self, id: G, graph: BTreeGraph<S, P, O>) -> Option<BTreeGraph<S, P, O>>
	where
		G: Ord,
	{
		self.named.insert(id, graph)
	}

	pub fn into_graph(mut self, id: Option<&G>) -> Option<BTreeGraph<S, P, O>>
	where
		G: Ord,
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

impl<S: Ord, P: Ord, O: Ord, G: Ord> BTreeDataset<S, P, O, G> {
	pub fn insert(&mut self, quad: Quad<S, P, O, G>) {
		let (subject, predicate, object, graph_name) = quad.into_parts();
		match self.graph_mut(graph_name.as_ref()) {
			Some(g) => g.insert(Triple(subject, predicate, object)),
			None => {
				let mut g = BTreeGraph::new();
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
					self.insert_graph(id.unwrap(), BTreeGraph::from_graph(graph));
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

impl<S: Ord, P: Ord, O: Ord, G: Ord> BTreeDataset<S, P, O, G>
where
	S: AsTerm,
	<S as AsTerm>::Iri: PartialEq,
	<S as AsTerm>::Literal: PartialEq,
	<S as AsTerm>::BlankId: Ord,
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
	) -> Option<BTreeBijection<'a, 'b, <S as AsTerm>::BlankId, <S as AsTerm>::BlankId>> {
		use crate::utils::isomorphism::btree::FindBTreeBlankIdBijection;

		fn has_no_blank<S: AsTerm, P: AsTerm, O: AsTerm, G: AsTerm>(
			Quad(s, p, o, g): &Quad<&S, &P, &O, &G>,
		) -> bool {
			!s.as_term().is_blank()
				&& !p.as_term().is_blank()
				&& !o.as_term().is_blank()
				&& !g.map(|g| g.as_term().is_blank()).unwrap_or(false)
		}

		let a_non_blank: BTreeSet<_> = self.quads().filter(has_no_blank).collect();
		let b_non_blank: BTreeSet<_> = other.quads().filter(has_no_blank).collect();

		if a_non_blank == b_non_blank {
			Self::find_btree_blank_id_bijection(self, other)
		} else {
			None
		}
	}
}

impl<S: Ord, P: Ord, O: Ord, G: Ord> crate::Dataset for BTreeDataset<S, P, O, G> {
	type Subject = S;
	type Predicate = P;
	type Object = O;
	type GraphLabel = G;

	type Graph = BTreeGraph<S, P, O>;
	type Graphs<'a>
	where
		Self: 'a,
		S: 'a,
		P: 'a,
		O: 'a,
		G: 'a,
	= Graphs<'a, S, P, O, G>;
	type Quads<'a>
	where
		Self: 'a,
		S: 'a,
		P: 'a,
		O: 'a,
	= Quads<'a, S, P, O, G>;

	fn graph(&self, id: Option<&G>) -> Option<&BTreeGraph<S, P, O>> {
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
	for BTreeDataset<S, P, O, G>
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
	default: Option<&'a BTreeGraph<S, P, O>>,
	it: std::collections::btree_map::Iter<'a, G, BTreeGraph<S, P, O>>,
}

impl<'a, S, P, O, G> Iterator for Graphs<'a, S, P, O, G> {
	type Item = (Option<&'a G>, &'a BTreeGraph<S, P, O>);

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
	default: Option<&'a mut BTreeGraph<S, P, O>>,
	it: std::collections::btree_map::IterMut<'a, G, BTreeGraph<S, P, O>>,
}

impl<'a, S, P, O, G> Iterator for GraphsMut<'a, S, P, O, G> {
	type Item = (Option<&'a G>, &'a mut BTreeGraph<S, P, O>);

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

impl<S: Clone + Ord, P: Clone + Ord, O: Ord, G: Clone + Ord> crate::SizedDataset
	for BTreeDataset<S, P, O, G>
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
	default: Option<BTreeGraph<S, P, O>>,
	it: std::collections::btree_map::IntoIter<G, BTreeGraph<S, P, O>>,
}

impl<S, P, O, G> Iterator for IntoGraphs<S, P, O, G> {
	type Item = (Option<G>, BTreeGraph<S, P, O>);

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

impl<S: Ord, P: Ord, O: Ord, G: Ord> crate::MutableDataset for BTreeDataset<S, P, O, G> {
	type GraphsMut<'a>
	where
		Self: 'a,
		S: 'a,
		P: 'a,
		O: 'a,
		G: 'a,
	= GraphsMut<'a, S, P, O, G>;

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

impl<S: Ord, P: Ord, O: Ord, G: Ord> std::iter::FromIterator<Quad<S, P, O, G>>
	for BTreeDataset<S, P, O, G>
{
	fn from_iter<I: IntoIterator<Item = Quad<S, P, O, G>>>(iter: I) -> Self {
		let mut ds = Self::new();
		ds.extend(iter);
		ds
	}
}

impl<S: Ord, P: Ord, O: Ord, G: Ord> std::iter::Extend<Quad<S, P, O, G>>
	for BTreeDataset<S, P, O, G>
{
	fn extend<I: IntoIterator<Item = Quad<S, P, O, G>>>(&mut self, iter: I) {
		for quad in iter {
			self.insert(quad);
		}
	}
}
