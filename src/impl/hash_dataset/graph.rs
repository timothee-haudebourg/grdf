use core::{fmt, hash::Hash};
use educe::Educe;
use hashbrown::{Equivalent, HashMap, HashSet};
use rdf_types::Triple;

use crate::GraphView;

use super::GraphPatternMatching;

/// Graph implementation based on `HashMap` and `HashSet`.
#[derive(Educe, Clone)]
#[educe(PartialEq(bound = "S: Eq + Hash, P: Eq + Hash, O: Eq + Hash"))]
#[educe(Eq(bound = "S: Eq + Hash, P: Eq + Hash, O: Eq + Hash"))]
#[educe(Default)]
pub struct HashGraph<S = rdf_types::Term, P = S, O = S> {
	pub(crate) table: HashMap<S, HashMap<P, HashSet<O>>>,
	len: usize,
}

impl<S, P, O> HashGraph<S, P, O> {
	/// Create a new empty `HashGraph`.
	pub fn new() -> Self {
		Self::default()
	}

	/// Returns the number of triples in the graph.
	pub fn len(&self) -> usize {
		self.len
	}

	/// Checks if the graph is empty.
	pub fn is_empty(&self) -> bool {
		self.len == 0
	}
}

impl<S: Eq + Hash, P: Eq + Hash, O: Eq + Hash> HashGraph<S, P, O> {
	/// Create a new `HashGraph` from another graph by consuming its triples.
	pub fn from_graph<G: crate::SizedGraph<Subject = S, Predicate = P, Object = O>>(
		g: G,
	) -> HashGraph<S, P, O> {
		let len = g.len();

		let mut subject_map = HashMap::new();
		for (subject, predicates) in g.into_subjects() {
			let mut predicate_map = HashMap::new();
			for (predicate, objects) in predicates {
				predicate_map.insert(predicate, objects.collect());
			}

			subject_map.insert(subject, predicate_map);
		}

		HashGraph {
			table: subject_map,
			len,
		}
	}

	pub fn insert(&mut self, triple: Triple<S, P, O>) -> bool {
		let (subject, predicate, object) = triple.into_parts();

		let added = self
			.table
			.entry(subject)
			.or_default()
			.entry(predicate)
			.or_default()
			.insert(object);

		if added {
			self.len += 1;
		}

		added
	}

	pub fn absorb<G: crate::SizedGraph<Subject = S, Predicate = P, Object = O>>(
		&mut self,
		other: G,
	) {
		let subjects = other.into_subjects();

		for (subject, predicates) in subjects {
			let this_predicates = self.table.entry(subject).or_default();
			for (predicate, objects) in predicates {
				let this_objects = this_predicates.entry(predicate).or_default();
				for object in objects {
					if this_objects.insert(object) {
						self.len += 1
					}
				}
			}
		}
	}

	pub fn remove<
		T: ?Sized + Equivalent<S> + Hash,
		U: ?Sized + Equivalent<P> + Hash,
		V: ?Sized + Equivalent<O> + Hash,
	>(
		&mut self,
		Triple(s, p, o): Triple<&T, &U, &V>,
	) {
		if let Some(predicates) = self.table.get_mut(s) {
			if let Some(objects) = predicates.get_mut(p) {
				if objects.remove(o) {
					self.len -= 1;
					if objects.is_empty() {
						predicates.remove(p).unwrap();
						if predicates.is_empty() {
							self.table.remove(s);
						}
					}
				}
			}
		}
	}

	pub fn take<
		T: ?Sized + Equivalent<S> + Hash,
		U: ?Sized + Equivalent<P> + Hash,
		V: ?Sized + Equivalent<O> + Hash,
	>(
		&mut self,
		Triple(s, p, o): Triple<&T, &U, &V>,
	) -> Option<Triple<S, P, O>>
	where
		S: Clone,
		P: Clone,
	{
		if let Some(predicates) = self.table.get_mut(s) {
			if let Some(objects) = predicates.get_mut(p) {
				if let Some(o) = objects.take(o) {
					let (s, p) = if objects.is_empty() {
						let p = predicates.remove_entry(p).unwrap().0;
						let s = if predicates.is_empty() {
							self.table.remove_entry(s).unwrap().0
						} else {
							self.table.get_key_value(s).unwrap().0.clone()
						};

						(s, p)
					} else {
						let p = predicates.get_key_value(p).unwrap().0.clone();
						let s = self.table.get_key_value(s).unwrap().0.clone();
						(s, p)
					};

					self.len -= 1;
					return Some(Triple(s, p, o));
				}
			}
		}

		None
	}

	pub fn take_match<
		T: ?Sized + Equivalent<S> + Hash,
		U: ?Sized + Equivalent<P> + Hash,
		V: ?Sized + Equivalent<O> + Hash,
	>(
		&mut self,
		Triple(s, p, o): Triple<Option<&T>, Option<&U>, Option<&V>>,
	) -> Option<Triple<S, P, O>>
	where
		S: Clone,
		P: Clone,
		O: Clone,
	{
		fn take_object_match<O: Clone + Eq + Hash, V: ?Sized + Equivalent<O> + Hash>(
			objects: &mut HashSet<O>,
			o: Option<&V>,
		) -> Option<O> {
			match o {
				Some(o) => objects.take(o),
				None => objects.iter().next().cloned().map(|o| {
					objects.remove::<O>(&o);
					o
				}),
			}
		}

		fn take_predicate_match<
			P: Clone + Eq + Hash,
			O: Clone + Eq + Hash,
			U: ?Sized + Equivalent<P> + Hash,
			V: ?Sized + Equivalent<O> + Hash,
		>(
			predicates: &mut HashMap<P, HashSet<O>>,
			p: Option<&U>,
			o: Option<&V>,
		) -> Option<(P, O)> {
			match p {
				Some(p) => {
					let objects = predicates.get_mut(p)?;
					let o = take_object_match(objects, o)?;

					let p = if objects.is_empty() {
						predicates.remove_entry(p).unwrap().0
					} else {
						predicates.get_key_value(p).unwrap().0.clone()
					};

					Some((p, o))
				}
				None => {
					for (p, objects) in &mut *predicates {
						if let Some(o) = take_object_match(objects, o) {
							let p = p.clone();

							if objects.is_empty() {
								predicates.remove::<P>(&p);
							}

							return Some((p, o));
						}
					}

					None
				}
			}
		}

		match s {
			Some(s) => {
				let predicates = self.table.get_mut(s)?;
				let (p, o) = take_predicate_match(predicates, p, o)?;

				let s = if predicates.is_empty() {
					self.table.remove_entry(s).unwrap().0
				} else {
					self.table.get_key_value(s).unwrap().0.clone()
				};

				self.len -= 1;
				Some(Triple(s, p, o))
			}
			None => {
				for (s, predicates) in &mut self.table {
					if let Some((p, o)) = take_predicate_match(predicates, p, o) {
						let s = s.clone();

						if predicates.is_empty() {
							self.table.remove::<S>(&s);
						}

						self.len -= 1;
						return Some(Triple(s, p, o));
					}
				}

				None
			}
		}
	}

	pub fn view<'a, A>(&'a self, subject: &'a S, access: A) -> GraphView<'a, Self, A> {
		crate::Graph::view(self, subject, access)
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

	type PatternMatching<'a, 'p> = GraphPatternMatching<'a, 'p, S, P, O> where
		Self: 'a,
		S: 'p,
		P: 'p,
		O: 'p;

	#[inline(always)]
	fn len(&self) -> usize {
		self.len()
	}

	fn triples(&self) -> Iter<S, P, O> {
		self.triples()
	}

	fn subjects(&self) -> Subjects<S, P, O> {
		self.subjects()
	}

	fn predicates(&self, subject: &S) -> Predicates<P, O> {
		self.predicates(subject)
	}

	fn objects(&self, subject: &S, predicate: &P) -> Objects<O> {
		self.objects(subject, predicate)
	}

	fn contains(&self, triple: Triple<&S, &P, &O>) -> bool {
		self.contains(triple)
	}

	fn pattern_matching<'p>(
		&self,
		pattern: Triple<
			Option<&'p Self::Subject>,
			Option<&'p Self::Predicate>,
			Option<&'p Self::Object>,
		>,
	) -> Self::PatternMatching<'_, 'p> {
		GraphPatternMatching::new(self, pattern)
	}
}

impl<S: Eq + Hash, P: Eq + Hash, O: Eq + Hash> HashGraph<S, P, O> {
	crate::macros::reflect_graph_impl!();
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

impl<S: Clone, P: Clone, O> IntoIterator for HashGraph<S, P, O> {
	type IntoIter = IntoIter<S, P, O>;
	type Item = Triple<S, P, O>;

	fn into_iter(self) -> Self::IntoIter {
		self.into_triples()
	}
}

impl<S: Eq + Hash, P: Eq + Hash, O: Eq + Hash> crate::MutableGraph for HashGraph<S, P, O> {
	fn insert(&mut self, triple: Triple<S, P, O>) -> bool {
		self.insert(triple)
	}

	fn remove(
		&mut self,
		triple: Triple<
			&<Self as crate::Graph>::Subject,
			&<Self as crate::Graph>::Predicate,
			&<Self as crate::Graph>::Object,
		>,
	) {
		self.remove(triple)
	}

	fn absorb<G: crate::SizedGraph<Subject = S, Predicate = P, Object = O>>(&mut self, other: G) {
		self.absorb(other)
	}
}

impl<
		S: Clone + Eq + Hash,
		P: Clone + Eq + Hash,
		O: Clone + Eq + Hash,
		T: ?Sized + Equivalent<S> + Hash,
		U: ?Sized + Equivalent<P> + Hash,
		V: ?Sized + Equivalent<O> + Hash,
	> crate::GraphTake<T, U, V> for HashGraph<S, P, O>
{
	fn take(
		&mut self,
		triple: Triple<&T, &U, &V>,
	) -> Option<Triple<Self::Subject, Self::Predicate, Self::Object>> {
		self.take(triple)
	}

	fn take_match(
		&mut self,
		triple: Triple<Option<&T>, Option<&U>, Option<&V>>,
	) -> Option<Triple<Self::Subject, Self::Predicate, Self::Object>> {
		self.take_match(triple)
	}
}

impl<S: fmt::Debug, P: fmt::Debug, O: fmt::Debug> fmt::Debug for HashGraph<S, P, O> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{{")?;

		for (i, rdf_types::Triple(s, p, o)) in self.triples().enumerate() {
			if i > 0 {
				write!(f, ",")?;
			}

			write!(f, " {s:?} {p:?} {o:?}")?;
		}

		write!(f, "  }}")
	}
}

#[derive(Educe)]
#[educe(Clone)]
pub struct Subjects<'a, S, P, O> {
	it: hashbrown::hash_map::Iter<'a, S, HashMap<P, HashSet<O>>>,
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
	it: hashbrown::hash_map::IntoIter<S, HashMap<P, HashSet<O>>>,
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

#[derive(Educe)]
#[educe(Clone)]
pub struct Predicates<'a, P, O> {
	it: Option<hashbrown::hash_map::Iter<'a, P, HashSet<O>>>,
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
	it: Option<hashbrown::hash_map::IntoIter<P, HashSet<O>>>,
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

#[derive(Educe)]
#[educe(Clone)]
pub struct Objects<'a, O> {
	it: Option<hashbrown::hash_set::Iter<'a, O>>,
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
	it: Option<hashbrown::hash_set::IntoIter<O>>,
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

#[derive(Educe)]
#[educe(Clone)]
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
