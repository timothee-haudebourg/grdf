//! Compatibility layer with the [`locspan`](https://crates.io/crates/locspan) library.
//!
//! Provides dataset implementations that also store the source location of each quad.
use locspan::{Loc, Location};
use rdf_types::loc::LocQuad;
use std::hash::Hash;

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

/// `HashDataset` with location information.
pub type HashDataset<S, P, O, G, F> = crate::HashDataset<S, P, Object<O, F>, G>;

impl<S: Eq + Hash, P: Eq + Hash, O: Eq + Hash, G: Eq + Hash, F: Eq + Hash>
	HashDataset<S, P, O, G, F>
{
	pub fn insert_loc(
		&mut self,
		Loc(rdf_types::Quad(Loc(s, s_loc), Loc(p, p_loc), Loc(o, o_loc), g), loc): LocQuad<
			S,
			P,
			O,
			G,
			F,
		>,
	) {
		let (locations, g) = match g {
			Some(Loc(g, g_loc)) => (Locations::new(s_loc, p_loc, o_loc, Some(g_loc)), Some(g)),
			None => (Locations::new(s_loc, p_loc, o_loc, None), None),
		};

		self.insert(rdf_types::Quad(
			s,
			p,
			Object {
				value: o,
				locations: Loc(locations, loc),
			},
			g,
		))
	}
}

impl<S: Eq + Hash, P: Eq + Hash, O: Eq + Hash, G: Eq + Hash, F: Eq + Hash>
	std::iter::Extend<LocQuad<S, P, O, G, F>> for HashDataset<S, P, O, G, F>
{
	fn extend<I: IntoIterator<Item = LocQuad<S, P, O, G, F>>>(&mut self, iter: I) {
		for quad in iter {
			self.insert_loc(quad)
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
	pub fn insert_loc(
		&mut self,
		Loc(rdf_types::Quad(Loc(s, s_loc), Loc(p, p_loc), Loc(o, o_loc), g), loc): LocQuad<
			S,
			P,
			O,
			G,
			F,
		>,
	) {
		let (locations, g) = match g {
			Some(Loc(g, g_loc)) => (Locations::new(s_loc, p_loc, o_loc, Some(g_loc)), Some(g)),
			None => (Locations::new(s_loc, p_loc, o_loc, None), None),
		};

		self.insert(rdf_types::Quad(
			s,
			p,
			Object {
				value: o,
				locations: Loc(locations, loc),
			},
			g,
		))
	}
}

impl<S: Ord, P: Ord, O: Ord, G: Ord, F: Ord> std::iter::Extend<LocQuad<S, P, O, G, F>>
	for BTreeDataset<S, P, O, G, F>
{
	fn extend<I: IntoIterator<Item = LocQuad<S, P, O, G, F>>>(&mut self, iter: I) {
		for quad in iter {
			self.insert_loc(quad)
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
