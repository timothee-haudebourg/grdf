//! Compatibility layer with the [`locspan`](https://crates.io/crates/locspan) library.
//!
//! Provides dataset implementations that also saves the source location of each quad.
use locspan::{Loc, Location};
use rdf_types::loc::LocQuad;
use std::hash::Hash;

/// Located [`Quad`](rdf_types::Quad) where each component if borrowed.
pub type LocQuadRef<'s, 'p, 'o, 'g, 'f, S, P, O, G, F> = Loc<
	rdf_types::Quad<Loc<&'s S, &'f F>, Loc<&'p P, &'f F>, Loc<&'o O, &'f F>, Loc<&'g G, &'f F>>,
	&'f F,
>;

/// Quad of [`Location`]s.
pub type Locations<F> = rdf_types::Quad<Location<F>, Location<F>, Location<F>, Location<F>>;

/// Quad object with location information for the entire quad.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Object<O, F> {
	/// Actual object value.
	pub value: O,

	/// Location information about the quad.
	///
	/// The outer [`Loc`] stores the position of the quad,
	/// while the inner [`Locations`] structure stores the
	/// position of each quad component.
	pub locations: Loc<Locations<F>, F>,
}

impl<O, F> Object<O, F> {
	/// Creates a new quad where all location information is stored in the `Object<O, F>`.
	pub fn new<S, P, G>(
		Loc(rdf_types::Quad(Loc(s, s_loc), Loc(p, p_loc), Loc(o, o_loc), g), loc): LocQuad<
			S,
			P,
			O,
			G,
			F,
		>,
	) -> rdf_types::Quad<S, P, Self, G> {
		let (locations, g) = match g {
			Some(Loc(g, g_loc)) => (Locations::new(s_loc, p_loc, o_loc, Some(g_loc)), Some(g)),
			None => (Locations::new(s_loc, p_loc, o_loc, None), None),
		};

		rdf_types::Quad(
			s,
			p,
			Object {
				value: o,
				locations: Loc(locations, loc),
			},
			g,
		)
	}

	pub fn as_loc_quad<'s, 'p, 'g, S, P, G>(
		&self,
		s: &'s S,
		p: &'p P,
		g: Option<&'g G>,
	) -> LocQuadRef<'s, 'p, '_, 'g, '_, S, P, O, G, F> {
		Loc(
			rdf_types::Quad(
				Loc(s, self.locations.subject().borrow()),
				Loc(p, self.locations.predicate().borrow()),
				Loc(&self.value, self.locations.object().borrow()),
				g.map(|g| Loc(g, self.locations.graph().unwrap().borrow())),
			),
			self.locations.location().borrow(),
		)
	}

	pub fn into_loc_quad<S, P, G>(self, s: S, p: P, g: Option<G>) -> LocQuad<S, P, O, G, F> {
		let Loc(locations, loc) = self.locations;
		let g_loc = locations.3;

		Loc(
			rdf_types::Quad(
				Loc(s, locations.0),
				Loc(p, locations.1),
				Loc(self.value, locations.2),
				g.map(|g| Loc(g, g_loc.unwrap())),
			),
			loc,
		)
	}
}

/// `HashDataset` with location information.
pub type HashDataset<S, P, O, G, F> = crate::HashDataset<S, P, Object<O, F>, G>;

impl<S: Eq + Hash, P: Eq + Hash, O: Eq + Hash, G: Eq + Hash, F: Eq + Hash>
	HashDataset<S, P, O, G, F>
{
	/// Inserts a located quad into the dataset.
	pub fn loc_insert(&mut self, quad: LocQuad<S, P, O, G, F>) {
		self.insert(Object::new(quad))
	}

	/// Creates a borrowing iterator over the located quads of the dataset.
	pub fn loc_quads(&self) -> LocQuads<Self> {
		LocQuads(self.quads())
	}

	/// Turns this dataset into an iterator over the located quads of the dataset.
	pub fn into_loc_quads(self) -> IntoLocQuads<Self>
	where
		S: Clone,
		P: Clone,
		G: Clone,
	{
		IntoLocQuads(self.into_quads())
	}
}

impl<S: Eq + Hash, P: Eq + Hash, O: Eq + Hash, G: Eq + Hash, F: Eq + Hash>
	std::iter::Extend<LocQuad<S, P, O, G, F>> for HashDataset<S, P, O, G, F>
{
	fn extend<I: IntoIterator<Item = LocQuad<S, P, O, G, F>>>(&mut self, iter: I) {
		for quad in iter {
			self.loc_insert(quad)
		}
	}
}

impl<S: Eq + Hash, P: Eq + Hash, O: Eq + Hash, G: Eq + Hash, F: Eq + Hash>
	std::iter::FromIterator<LocQuad<S, P, O, G, F>> for HashDataset<S, P, O, G, F>
{
	fn from_iter<I: IntoIterator<Item = LocQuad<S, P, O, G, F>>>(iter: I) -> Self {
		let mut dataset = Self::new();
		dataset.extend(iter);
		dataset
	}
}

/// `BTreeDataset` with location information.
pub type BTreeDataset<S, P, O, G, F> = crate::BTreeDataset<S, P, Object<O, F>, G>;

impl<S: Ord, P: Ord, O: Ord, G: Ord, F: Ord> BTreeDataset<S, P, O, G, F> {
	/// Inserts a located quad into the dataset.
	pub fn loc_insert(&mut self, quad: LocQuad<S, P, O, G, F>) {
		self.insert(Object::new(quad))
	}

	/// Creates a borrowing iterator over the located quads of the dataset.
	pub fn loc_quads(&self) -> LocQuads<Self> {
		LocQuads(self.quads())
	}

	/// Turns this dataset into an iterator over the located quads of the dataset.
	pub fn into_loc_quads(self) -> IntoLocQuads<Self>
	where
		S: Clone,
		P: Clone,
		G: Clone,
	{
		IntoLocQuads(self.into_quads())
	}
}

impl<S: Ord, P: Ord, O: Ord, G: Ord, F: Ord> std::iter::Extend<LocQuad<S, P, O, G, F>>
	for BTreeDataset<S, P, O, G, F>
{
	fn extend<I: IntoIterator<Item = LocQuad<S, P, O, G, F>>>(&mut self, iter: I) {
		for quad in iter {
			self.loc_insert(quad)
		}
	}
}

impl<S: Ord, P: Ord, O: Ord, G: Ord, F: Ord> std::iter::FromIterator<LocQuad<S, P, O, G, F>>
	for BTreeDataset<S, P, O, G, F>
{
	fn from_iter<I: IntoIterator<Item = LocQuad<S, P, O, G, F>>>(iter: I) -> Self {
		let mut dataset = Self::new();
		dataset.extend(iter);
		dataset
	}
}

pub struct LocQuads<'a, D: 'a + crate::Dataset>(D::Quads<'a>);

impl<'a, O: 'a, F: 'a, D: 'a + crate::Dataset<Object = Object<O, F>>> Iterator for LocQuads<'a, D> {
	type Item = Loc<
		rdf_types::Quad<
			Loc<&'a D::Subject, &'a F>,
			Loc<&'a D::Predicate, &'a F>,
			Loc<&'a O, &'a F>,
			Loc<&'a D::GraphLabel, &'a F>,
		>,
		&'a F,
	>;

	fn next(&mut self) -> Option<Self::Item> {
		self.0
			.next()
			.map(|rdf_types::Quad(s, p, o, g)| o.as_loc_quad(s, p, g))
	}
}

pub struct IntoLocQuads<D: crate::SizedDataset>(D::IntoQuads)
where
	D::Graph: crate::SizedGraph;

impl<O, F, D: crate::SizedDataset<Object = Object<O, F>>> Iterator for IntoLocQuads<D>
where
	D::Graph: crate::SizedGraph,
{
	type Item = LocQuad<D::Subject, D::Predicate, O, D::GraphLabel, F>;

	fn next(&mut self) -> Option<Self::Item> {
		self.0
			.next()
			.map(|rdf_types::Quad(s, p, o, g)| o.into_loc_quad(s, p, g))
	}
}
