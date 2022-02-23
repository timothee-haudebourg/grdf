//! Dataset implementation based on `BTreeMap` and `BTreeSet`.
use crate::{Quad, Triple};
use std::collections::{BTreeMap, BTreeSet};

/// Graph implementation based on `BTreeMap` and `BTreeSet`.
pub struct BTreeGraph<T: Ord = rdf_types::Term> {
	table: BTreeMap<T, BTreeMap<T, BTreeSet<T>>>,
}

impl<T: Ord> BTreeGraph<T> {
	/// Create a new empty `BTreeGraph`.
	pub fn new() -> BTreeGraph<T> {
		Self::default()
	}

	/// Create a new `BTreeGraph` from another graph by consuming its triples.
	pub fn from_graph<G: crate::SizedGraph<T>>(g: G) -> BTreeGraph<T> {
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
}

impl<T: Ord> Default for BTreeGraph<T> {
	fn default() -> BTreeGraph<T> {
		BTreeGraph {
			table: BTreeMap::new(),
		}
	}
}

impl<T: Ord> crate::Graph<T> for BTreeGraph<T> {
	type Objects<'a>
	where
		T: 'a,
	= Objects<'a, T>;
	type Predicates<'a>
	where
		T: 'a,
	= Predicates<'a, T>;
	type Subjects<'a>
	where
		T: 'a,
	= Subjects<'a, T>;
	type Triples<'a>
	where
		T: 'a,
	= Iter<'a, T>;

	fn triples<'a>(&'a self) -> Iter<'a, T>
	where
		T: 'a,
	{
		Iter {
			subjects: self.subjects(),
			subject: None,
			predicates: None,
			predicate: None,
			objects: None,
		}
	}

	fn subjects<'a>(&'a self) -> Subjects<'a, T>
	where
		T: 'a,
	{
		Subjects {
			it: self.table.iter(),
		}
	}

	fn predicates<'a>(&'a self, subject: &T) -> Predicates<'a, T>
	where
		T: 'a,
	{
		match self.table.get(subject) {
			Some(map) => Predicates {
				it: Some(map.iter()),
			},
			None => Predicates { it: None },
		}
	}

	fn objects<'a>(&'a self, subject: &T, predicate: &T) -> Objects<'a, T>
	where
		T: 'a,
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

	fn contains(&self, Triple(subject, predicate, object): Triple<&T, &T, &T>) -> bool {
		match self.table.get(subject) {
			Some(map) => match map.get(predicate) {
				Some(map) => map.contains(object),
				None => false,
			},
			None => false,
		}
	}
}

impl<'a, T: 'a + Ord> std::iter::IntoIterator for &'a BTreeGraph<T> {
	type IntoIter = Iter<'a, T>;
	type Item = Triple<&'a T, &'a T, &'a T>;

	fn into_iter(self) -> Self::IntoIter {
		use crate::Graph;
		self.triples()
	}
}

impl<T: Clone + Ord> crate::SizedGraph<T> for BTreeGraph<T> {
	type IntoObjects = IntoObjects<T>;
	type IntoPredicates = IntoPredicates<T>;
	type IntoSubjects = IntoSubjects<T>;
	type IntoTriples = IntoIter<T>;

	fn into_triples(self) -> IntoIter<T> {
		IntoIter {
			subjects: self.into_subjects(),
			subject: None,
			predicates: None,
			predicate: None,
			objects: None,
		}
	}

	fn into_subjects(self) -> IntoSubjects<T> {
		IntoSubjects {
			it: self.table.into_iter(),
		}
	}

	fn into_predicates(mut self, subject: &T) -> IntoPredicates<T> {
		match self.table.remove(subject) {
			Some(map) => IntoPredicates {
				it: Some(map.into_iter()),
			},
			None => IntoPredicates { it: None },
		}
	}

	fn into_objects(mut self, subject: &T, predicate: &T) -> IntoObjects<T> {
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

impl<T: Clone + Ord> std::iter::IntoIterator for BTreeGraph<T> {
	type IntoIter = IntoIter<T>;
	type Item = Triple<T, T, T>;

	fn into_iter(self) -> Self::IntoIter {
		use crate::SizedGraph;
		self.into_triples()
	}
}

impl<T: Ord> crate::MutableGraph<T> for BTreeGraph<T> {
	fn insert(&mut self, triple: Triple<T, T, T>) {
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

	fn absorb<G: crate::SizedGraph<T>>(&mut self, other: G) {
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

pub struct Subjects<'a, T: Ord> {
	it: std::collections::btree_map::Iter<'a, T, BTreeMap<T, BTreeSet<T>>>,
}

impl<'a, T: Ord> Iterator for Subjects<'a, T> {
	type Item = (&'a T, Predicates<'a, T>);

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

pub struct IntoSubjects<T: Ord> {
	it: std::collections::btree_map::IntoIter<T, BTreeMap<T, BTreeSet<T>>>,
}

impl<T: Ord> Iterator for IntoSubjects<T> {
	type Item = (T, IntoPredicates<T>);

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

pub struct Predicates<'a, T: Ord> {
	it: Option<std::collections::btree_map::Iter<'a, T, BTreeSet<T>>>,
}

impl<'a, T: Ord> Iterator for Predicates<'a, T> {
	type Item = (&'a T, Objects<'a, T>);

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

pub struct IntoPredicates<T: Ord> {
	it: Option<std::collections::btree_map::IntoIter<T, BTreeSet<T>>>,
}

impl<T: Ord> Iterator for IntoPredicates<T> {
	type Item = (T, IntoObjects<T>);

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

pub struct Objects<'a, T: Ord> {
	it: Option<std::collections::btree_set::Iter<'a, T>>,
}

impl<'a, T: Ord> Iterator for Objects<'a, T> {
	type Item = &'a T;

	fn next(&mut self) -> Option<Self::Item> {
		match &mut self.it {
			Some(it) => it.next(),
			None => None,
		}
	}
}

pub struct IntoObjects<T: Ord> {
	it: Option<std::collections::btree_set::IntoIter<T>>,
}

impl<T: Ord> Iterator for IntoObjects<T> {
	type Item = T;

	fn next(&mut self) -> Option<Self::Item> {
		match &mut self.it {
			Some(it) => it.next(),
			None => None,
		}
	}
}

pub struct Iter<'a, T: Ord> {
	subjects: Subjects<'a, T>,
	subject: Option<&'a T>,
	predicates: Option<Predicates<'a, T>>,
	predicate: Option<&'a T>,
	objects: Option<Objects<'a, T>>,
}

impl<'a, T: Ord> Iterator for Iter<'a, T> {
	type Item = Triple<&'a T, &'a T, &'a T>;

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

pub struct IntoIter<T: Ord> {
	subjects: IntoSubjects<T>,
	subject: Option<T>,
	predicates: Option<IntoPredicates<T>>,
	predicate: Option<T>,
	objects: Option<IntoObjects<T>>,
}

impl<T: Clone + Ord> Iterator for IntoIter<T> {
	type Item = Triple<T, T, T>;

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

pub struct BTreeDataset<T: Ord> {
	default: BTreeGraph<T>,
	named: BTreeMap<T, BTreeGraph<T>>,
}

impl<T: 'static + Ord> crate::Dataset<T> for BTreeDataset<T> {
	type Graph = BTreeGraph<T>;
	type Graphs<'a> = Graphs<'a, T>;
	type Quads<'a> = Quads<'a, T>;

	fn graph(&self, id: Option<&T>) -> Option<&BTreeGraph<T>> {
		match id {
			Some(id) => self.named.get(id),
			None => Some(&self.default),
		}
	}

	fn graphs(&self) -> Graphs<T> {
		Graphs {
			default: Some(&self.default),
			it: self.named.iter(),
		}
	}

	fn quads(&self) -> Quads<T> {
		Quads {
			graphs: self.graphs(),
			graph: None,
			triples: None,
		}
	}
}

pub struct Graphs<'a, T: Ord> {
	default: Option<&'a BTreeGraph<T>>,
	it: std::collections::btree_map::Iter<'a, T, BTreeGraph<T>>,
}

impl<'a, T: Ord> Iterator for Graphs<'a, T> {
	type Item = (Option<&'a T>, &'a BTreeGraph<T>);

	fn next(&mut self) -> Option<Self::Item> {
		if let Some(default) = self.default {
			self.default = None;
			Some((None, default))
		} else {
			self.it.next().map(|(id, graph)| (Some(id), graph))
		}
	}
}

pub struct GraphsMut<'a, T: Ord> {
	default: Option<&'a mut BTreeGraph<T>>,
	it: std::collections::btree_map::IterMut<'a, T, BTreeGraph<T>>,
}

impl<'a, T: Ord> Iterator for GraphsMut<'a, T> {
	type Item = (Option<&'a T>, &'a mut BTreeGraph<T>);

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

pub struct Quads<'a, T: Ord> {
	graphs: Graphs<'a, T>,
	graph: Option<&'a T>,
	triples: Option<Iter<'a, T>>,
}

impl<'a, T: Ord> Iterator for Quads<'a, T> {
	type Item = Quad<&'a T, &'a T, &'a T, &'a T>;

	fn next(&mut self) -> Option<Quad<&'a T, &'a T, &'a T, &'a T>> {
		use crate::Graph;
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

impl<T: 'static + Clone + Ord> crate::SizedDataset<T> for BTreeDataset<T> {
	type IntoGraphs = IntoGraphs<T>;
	type IntoQuads = IntoQuads<T>;

	fn into_graph(mut self, id: Option<&T>) -> Option<Self::Graph> {
		match id {
			Some(id) => self.named.remove(id),
			None => Some(self.default),
		}
	}

	fn into_graphs(self) -> Self::IntoGraphs {
		IntoGraphs {
			default: Some(self.default),
			it: self.named.into_iter(),
		}
	}

	fn into_quads(self) -> Self::IntoQuads {
		IntoQuads {
			graphs: self.into_graphs(),
			graph: None,
			triples: None,
		}
	}
}

pub struct IntoGraphs<T: Ord> {
	default: Option<BTreeGraph<T>>,
	it: std::collections::btree_map::IntoIter<T, BTreeGraph<T>>,
}

impl<T: Ord> Iterator for IntoGraphs<T> {
	type Item = (Option<T>, BTreeGraph<T>);

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

pub struct IntoQuads<T: Clone + Ord = rdf_types::Term> {
	graphs: IntoGraphs<T>,
	graph: Option<T>,
	triples: Option<IntoIter<T>>,
}

impl<T: Clone + Ord> Iterator for IntoQuads<T> {
	type Item = Quad<T, T, T, T>;

	fn next(&mut self) -> Option<Quad<T, T, T, T>> {
		use crate::SizedGraph;
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

impl<T: Ord> BTreeDataset<T> {
	pub fn new() -> Self {
		Self::default()
	}
}

impl<T: 'static + Ord> crate::MutableDataset<T> for BTreeDataset<T> {
	type GraphsMut<'a> = GraphsMut<'a, T>;

	fn graph_mut(&mut self, id: Option<&T>) -> Option<&mut Self::Graph> {
		match id {
			Some(id) => self.named.get_mut(id),
			None => Some(&mut self.default),
		}
	}

	fn graphs_mut(&mut self) -> Self::GraphsMut<'_> {
		GraphsMut {
			default: Some(&mut self.default),
			it: self.named.iter_mut(),
		}
	}

	fn insert_graph(&mut self, id: T, graph: Self::Graph) -> Option<Self::Graph> {
		self.named.insert(id, graph)
	}

	fn insert(&mut self, quad: Quad<T, T, T, T>) {
		use crate::MutableGraph;
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

	fn absorb<D: crate::SizedDataset<T>>(&mut self, other: D)
	where
		D::Graph: crate::SizedGraph<T>,
	{
		use crate::MutableGraph;
		for (id, graph) in other.into_graphs() {
			match self.graph_mut(id.as_ref()) {
				Some(g) => g.absorb(graph),
				None => {
					self.insert_graph(id.unwrap(), BTreeGraph::from_graph(graph));
				}
			}
		}
	}
}

impl<T: Ord> Default for BTreeDataset<T> {
	fn default() -> BTreeDataset<T> {
		BTreeDataset {
			default: BTreeGraph::default(),
			named: BTreeMap::new(),
		}
	}
}