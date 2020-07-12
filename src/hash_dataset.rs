//! Dataset implementation based on `HashMap` and `HashSet`.

use crate::{Quad, Triple};
use std::collections::{HashMap, HashSet};
use std::hash::Hash;

/// Graph implementation based on `HashMap` and `HashSet`.
pub struct HashGraph<T: Hash + Eq = crate::Term> {
	table: HashMap<T, HashMap<T, HashSet<T>>>,
}

impl<T: Hash + Eq> HashGraph<T> {
	/// Create a new empty `HashGraph`.
	pub fn new() -> HashGraph<T> { Self::default() }

	/// Create a new `HashGraph` from another graph by consuming its triples.
	pub fn from_graph<G: crate::SizedGraph<T>>(g: G) -> HashGraph<T> {
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
}

impl<T: Hash + Eq> Default for HashGraph<T> {
	fn default() -> HashGraph<T> {
		HashGraph {
			table: HashMap::new(),
		}
	}
}

impl<T: Hash + Eq> crate::Graph<T> for HashGraph<T> {
	type Objects<'a> where T: 'a = Objects<'a, T>;
	type Predicates<'a> where T: 'a = Predicates<'a, T>;
	type Subjects<'a> where T: 'a = Subjects<'a, T>;
	type Triples<'a> where T: 'a = Iter<'a, T>;

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

	fn contains(&self, Triple(subject, predicate, object): Triple<&T>) -> bool {
		match self.table.get(subject) {
			Some(map) => match map.get(predicate) {
				Some(map) => map.contains(object),
				None => false,
			},
			None => false,
		}
	}
}

impl<'a, T: 'a + Hash + Eq> std::iter::IntoIterator for &'a HashGraph<T> {
	type IntoIter = Iter<'a, T>;
	type Item = Triple<&'a T>;

	fn into_iter(self) -> Self::IntoIter {
		use crate::Graph;
		self.triples()
	}
}

impl<T: Clone + Hash + Eq> crate::SizedGraph<T> for HashGraph<T> {
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

impl<T: Clone + Hash + Eq> std::iter::IntoIterator for HashGraph<T> {
	type IntoIter = IntoIter<T>;
	type Item = Triple<T>;

	fn into_iter(self) -> Self::IntoIter {
		use crate::SizedGraph;
		self.into_triples()
	}
}

impl<T: Hash + Eq> crate::MutableGraph<T> for HashGraph<T> {
	fn insert(&mut self, triple: Triple<T>) {
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

pub struct Subjects<'a, T: Hash + Eq> {
	it: std::collections::hash_map::Iter<'a, T, HashMap<T, HashSet<T>>>,
}

impl<'a, T: Hash + Eq> Iterator for Subjects<'a, T> {
	type Item = (&'a T, Predicates<'a, T>);

	fn next(&mut self) -> Option<Self::Item> {
		match self.it.next() {
			Some((subject, map)) => Some((
				subject,
				Predicates {
					it: Some(map.iter()),
				},
			)),
			None => None,
		}
	}
}

pub struct IntoSubjects<T: Hash + Eq> {
	it: std::collections::hash_map::IntoIter<T, HashMap<T, HashSet<T>>>,
}

impl<T: Hash + Eq> Iterator for IntoSubjects<T> {
	type Item = (T, IntoPredicates<T>);

	fn next(&mut self) -> Option<Self::Item> {
		match self.it.next() {
			Some((subject, map)) => Some((
				subject,
				IntoPredicates {
					it: Some(map.into_iter()),
				},
			)),
			None => None,
		}
	}
}

pub struct Predicates<'a, T: Hash + Eq> {
	it: Option<std::collections::hash_map::Iter<'a, T, HashSet<T>>>,
}

impl<'a, T: Hash + Eq> Iterator for Predicates<'a, T> {
	type Item = (&'a T, Objects<'a, T>);

	fn next(&mut self) -> Option<Self::Item> {
		match &mut self.it {
			Some(it) => match it.next() {
				Some((predicate, set)) => Some((
					predicate,
					Objects {
						it: Some(set.iter()),
					},
				)),
				None => None,
			},
			None => None,
		}
	}
}

pub struct IntoPredicates<T: Hash + Eq> {
	it: Option<std::collections::hash_map::IntoIter<T, HashSet<T>>>,
}

impl<T: Hash + Eq> Iterator for IntoPredicates<T> {
	type Item = (T, IntoObjects<T>);

	fn next(&mut self) -> Option<Self::Item> {
		match &mut self.it {
			Some(it) => match it.next() {
				Some((predicate, set)) => Some((
					predicate,
					IntoObjects {
						it: Some(set.into_iter()),
					},
				)),
				None => None,
			},
			None => None,
		}
	}
}

pub struct Objects<'a, T: Hash + Eq> {
	it: Option<std::collections::hash_set::Iter<'a, T>>,
}

impl<'a, T: Hash + Eq> Iterator for Objects<'a, T> {
	type Item = &'a T;

	fn next(&mut self) -> Option<Self::Item> {
		match &mut self.it {
			Some(it) => it.next(),
			None => None,
		}
	}
}

pub struct IntoObjects<T: Hash + Eq> {
	it: Option<std::collections::hash_set::IntoIter<T>>,
}

impl<T: Hash + Eq> Iterator for IntoObjects<T> {
	type Item = T;

	fn next(&mut self) -> Option<Self::Item> {
		match &mut self.it {
			Some(it) => it.next(),
			None => None,
		}
	}
}

pub struct Iter<'a, T: Hash + Eq> {
	subjects: Subjects<'a, T>,
	subject: Option<&'a T>,
	predicates: Option<Predicates<'a, T>>,
	predicate: Option<&'a T>,
	objects: Option<Objects<'a, T>>,
}

impl<'a, T: Hash + Eq> Iterator for Iter<'a, T> {
	type Item = Triple<&'a T>;

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

pub struct IntoIter<T: Hash + Eq> {
	subjects: IntoSubjects<T>,
	subject: Option<T>,
	predicates: Option<IntoPredicates<T>>,
	predicate: Option<T>,
	objects: Option<IntoObjects<T>>,
}

impl<T: Clone + Hash + Eq> Iterator for IntoIter<T> {
	type Item = Triple<T>;

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

pub struct HashDataset<T: Hash + Eq> {
	default: HashGraph<T>,
	named: HashMap<T, HashGraph<T>>,
}

impl<T: 'static + Hash + Eq> crate::Dataset<T> for HashDataset<T> {
	type Graph = HashGraph<T>;
	type Graphs<'a> = Graphs<'a, T>;
	type Quads<'a> = Quads<'a, T>;

	fn graph(&self, id: Option<&T>) -> Option<&HashGraph<T>> {
		match id {
			Some(id) => self.named.get(id),
			None => Some(&self.default),
		}
	}

	fn graphs<'a>(&'a self) -> Graphs<'a, T> {
		Graphs {
			default: Some(&self.default),
			it: self.named.iter(),
		}
	}

	fn quads<'a>(&'a self) -> Quads<'a, T> {
		Quads {
			graphs: self.graphs(),
			graph: None,
			triples: None,
		}
	}
}

pub struct Graphs<'a, T: Hash + Eq> {
	default: Option<&'a HashGraph<T>>,
	it: std::collections::hash_map::Iter<'a, T, HashGraph<T>>,
}

impl<'a, T: Hash + Eq> Iterator for Graphs<'a, T> {
	type Item = (Option<&'a T>, &'a HashGraph<T>);

	fn next(&mut self) -> Option<Self::Item> {
		if let Some(default) = self.default {
			self.default = None;
			Some((None, default))
		} else {
			match self.it.next() {
				Some((id, graph)) => Some((Some(id), graph)),
				None => None,
			}
		}
	}
}

pub struct GraphsMut<'a, T: Hash + Eq> {
	default: Option<&'a mut HashGraph<T>>,
	it: std::collections::hash_map::IterMut<'a, T, HashGraph<T>>,
}

impl<'a, T: Hash + Eq> Iterator for GraphsMut<'a, T> {
	type Item = (Option<&'a T>, &'a mut HashGraph<T>);

	fn next(&mut self) -> Option<Self::Item> {
		let mut default = None;
		std::mem::swap(&mut default, &mut self.default);
		if let Some(default) = default {
			self.default = None;
			Some((None, default))
		} else {
			match self.it.next() {
				Some((id, graph)) => Some((Some(id), graph)),
				None => None,
			}
		}
	}
}

pub struct Quads<'a, T: Hash + Eq> {
	graphs: Graphs<'a, T>,
	graph: Option<&'a T>,
	triples: Option<Iter<'a, T>>,
}

impl<'a, T: Hash + Eq> Iterator for Quads<'a, T> {
	type Item = Quad<&'a T>;

	fn next(&mut self) -> Option<Quad<&'a T>> {
		use crate::Graph;
		loop {
			match &mut self.triples {
				Some(triples) => match triples.next() {
					Some(triple) => return Some(Quad(self.graph, triple.0, triple.1, triple.2)),
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

impl<T: 'static + Clone + Hash + Eq> crate::SizedDataset<T> for HashDataset<T> {
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

pub struct IntoGraphs<T: Hash + Eq> {
	default: Option<HashGraph<T>>,
	it: std::collections::hash_map::IntoIter<T, HashGraph<T>>,
}

impl<T: Hash + Eq> Iterator for IntoGraphs<T> {
	type Item = (Option<T>, HashGraph<T>);

	fn next(&mut self) -> Option<Self::Item> {
		let mut default = None;
		std::mem::swap(&mut default, &mut self.default);
		if let Some(default) = default {
			self.default = None;
			Some((None, default))
		} else {
			match self.it.next() {
				Some((id, graph)) => Some((Some(id), graph)),
				None => None,
			}
		}
	}
}

pub struct IntoQuads<T: Clone + Hash + Eq = crate::Term> {
	graphs: IntoGraphs<T>,
	graph: Option<T>,
	triples: Option<IntoIter<T>>,
}

impl<T: Clone + Hash + Eq> Iterator for IntoQuads<T> {
	type Item = Quad<T>;

	fn next(&mut self) -> Option<Quad<T>> {
		use crate::SizedGraph;
		loop {
			match &mut self.triples {
				Some(triples) => match triples.next() {
					Some(triple) => {
						return Some(Quad(self.graph.clone(), triple.0, triple.1, triple.2))
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

impl<T: Hash + Eq> HashDataset<T> {
	pub fn new() -> Self { Self::default() }
}

impl<T: 'static + Hash + Eq> crate::MutableDataset<T> for HashDataset<T> {
	type GraphsMut<'a> = GraphsMut<'a, T>;

	fn graph_mut(&mut self, id: Option<&T>) -> Option<&mut Self::Graph> {
		match id {
			Some(id) => self.named.get_mut(id),
			None => Some(&mut self.default),
		}
	}

	fn graphs_mut<'a>(&'a mut self) -> Self::GraphsMut<'a> {
		GraphsMut {
			default: Some(&mut self.default),
			it: self.named.iter_mut(),
		}
	}

	fn insert_graph(&mut self, id: T, graph: Self::Graph) -> Option<Self::Graph> {
		self.named.insert(id, graph)
	}

	fn insert(&mut self, quad: Quad<T>) {
		use crate::MutableGraph;
		let (graph_name, subject, predicate, object) = quad.into_parts();
		match self.graph_mut(graph_name.as_ref()) {
			Some(g) => g.insert(Triple(subject, predicate, object)),
			None => {
				let mut g = HashGraph::new();
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
					self.insert_graph(id.unwrap(), HashGraph::from_graph(graph));
				}
			}
		}
	}
}

impl<T: Hash + Eq> Default for HashDataset<T> {
	fn default() -> HashDataset<T> {
		HashDataset {
			default: HashGraph::default(),
			named: HashMap::new(),
		}
	}
}
