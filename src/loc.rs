use std::hash::Hash;
use locspan::{Loc, Location};

pub type Locations<F> = rdf_types::Quad<Location<F>, Location<F>, Location<F>, Location<F>>;

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Object<O, F> {
	pub value: O,
	pub locations: Loc<Locations<F>, F>
}

pub type HashDataset<S, P, O, G, F> = crate::HashDataset<S, P, Object<O, F>, G>;

impl<S: Eq + Hash, P: Eq + Hash, O: Eq + Hash, G: Eq + Hash, F: Eq + Hash> std::iter::FromIterator<Loc<rdf_types::Quad<Loc<S, F>, Loc<P, F>, Loc<O, F>, Loc<G, F>>, F>> for HashDataset<S, P, O, G, F> {
	fn from_iter<I: IntoIterator<Item=Loc<rdf_types::Quad<Loc<S, F>, Loc<P, F>, Loc<O, F>, Loc<G, F>>, F>>>(iter: I) -> Self {
		let mut dataset = Self::new();
		
		for Loc(rdf_types::Quad(Loc(s, s_loc), Loc(p, p_loc), Loc(o, o_loc), g), loc) in iter {
			let (locations, g) = match g {
				Some(Loc(g, g_loc)) => {
					(Locations::new(s_loc, p_loc, o_loc, Some(g_loc)), Some(g))
				},
				None => {
					(Locations::new(s_loc, p_loc, o_loc, None), None)
				}
			};
			
			dataset.insert(rdf_types::Quad(
				s,
				p,
				Object {
					value: o,
					locations: Loc(locations, loc)
				},
				g
			))
		}

		dataset
	}
}

pub type BTreeDataset<S, P, O, G, F> = crate::BTreeDataset<S, P, Object<O, F>, G>;

impl<S: Ord, P: Ord, O: Ord, G: Ord, F: Ord> std::iter::FromIterator<Loc<rdf_types::Quad<Loc<S, F>, Loc<P, F>, Loc<O, F>, Loc<G, F>>, F>> for BTreeDataset<S, P, O, G, F> {
	fn from_iter<I: IntoIterator<Item=Loc<rdf_types::Quad<Loc<S, F>, Loc<P, F>, Loc<O, F>, Loc<G, F>>, F>>>(iter: I) -> Self {
		let mut dataset = Self::new();
		
		for Loc(rdf_types::Quad(Loc(s, s_loc), Loc(p, p_loc), Loc(o, o_loc), g), loc) in iter {
			let (locations, g) = match g {
				Some(Loc(g, g_loc)) => {
					(Locations::new(s_loc, p_loc, o_loc, Some(g_loc)), Some(g))
				},
				None => {
					(Locations::new(s_loc, p_loc, o_loc, None), None)
				}
			};
			
			dataset.insert(rdf_types::Quad(
				s,
				p,
				Object {
					value: o,
					locations: Loc(locations, loc)
				},
				g
			))
		}

		dataset
	}
}