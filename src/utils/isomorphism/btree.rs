use super::blank_term_matches;
use crate::{Dataset, Quad};
use derivative::Derivative;
use rdf_types::{AsRdfTerm, BlankIdBuf, Id};
use std::collections::btree_map::Entry;
use std::collections::{BTreeMap, BTreeSet};

pub(crate) trait FindBTreeBlankIdBijection<V: Dataset>: Dataset {
	fn blank_quad_matches<'u, 'v, UI, UB, UL, VI, VB, VL>(
		a: Quad<&'u Self::Subject, &'u Self::Predicate, &'u Self::Object, &'u Self::GraphLabel>,
		b: Quad<&'v V::Subject, &'v V::Predicate, &'v V::Object, &'v V::GraphLabel>,
	) -> bool
	where
		Self::Subject: AsRdfTerm<UI, UB, UL>,
		Self::Predicate: AsRdfTerm<UI, UB, UL>,
		Self::Object: AsRdfTerm<UI, UB, UL>,
		Self::GraphLabel: AsRdfTerm<UI, UB, UL>,
		V::Subject: AsRdfTerm<VI, VB, VL>,
		V::Predicate: AsRdfTerm<VI, VB, VL>,
		V::Object: AsRdfTerm<VI, VB, VL>,
		V::GraphLabel: AsRdfTerm<VI, VB, VL>,
		UI: PartialEq<VI>,
		UL: PartialEq<VL>;

	fn blank_quad_matches_with<'u, 'v, UI, UB, UL, VI, VB, VL>(
		sigma: &BTreeBijection<'u, 'v, UB, VB>,
		a: Quad<&'u Self::Subject, &'u Self::Predicate, &'u Self::Object, &'u Self::GraphLabel>,
		b: Quad<&'v V::Subject, &'v V::Predicate, &'v V::Object, &'v V::GraphLabel>,
	) -> bool
	where
		Self::Subject: AsRdfTerm<UI, UB, UL>,
		Self::Predicate: AsRdfTerm<UI, UB, UL>,
		Self::Object: AsRdfTerm<UI, UB, UL>,
		Self::GraphLabel: AsRdfTerm<UI, UB, UL>,
		V::Subject: AsRdfTerm<VI, VB, VL>,
		V::Predicate: AsRdfTerm<VI, VB, VL>,
		V::Object: AsRdfTerm<VI, VB, VL>,
		V::GraphLabel: AsRdfTerm<VI, VB, VL>,
		UI: PartialEq<VI>,
		UL: PartialEq<VL>,
		UB: Ord,
		VB: Ord;

	/// Find a blank node identifier substitution from the blank no identifiers of `a` to the blank node identifiers of `b`
	/// so that the sub-dataset of `a` containing all the quads with blank node identifier
	/// is equal to the sub-dataset of `b` containing all the quads with a blank node identifier
	/// once the substitution if applied.
	///
	/// The function ignores all the quads having no blank node identifiers.
	#[allow(clippy::type_complexity)]
	fn find_btree_blank_id_bijection<'u, 'v, UI: 'u, UB, UL: 'u, VI: 'v, VB, VL: 'v>(
		a: &'u Self,
		b: &'v V,
	) -> Option<BTreeBijection<'u, 'v, UB, VB>>
	where
		Self::Subject: Ord + AsRdfTerm<UI, UB, UL>,
		Self::Predicate: Ord + AsRdfTerm<UI, UB, UL>,
		Self::Object: Ord + AsRdfTerm<UI, UB, UL>,
		Self::GraphLabel: Ord + AsRdfTerm<UI, UB, UL>,
		V::Subject: Ord + AsRdfTerm<VI, VB, VL>,
		V::Predicate: Ord + AsRdfTerm<VI, VB, VL>,
		V::Object: Ord + AsRdfTerm<VI, VB, VL>,
		V::GraphLabel: Ord + AsRdfTerm<VI, VB, VL>,
		UI: PartialEq<VI>,
		UL: PartialEq<VL>,
		UB: Ord,
		VB: Ord;

	#[allow(clippy::type_complexity)]
	fn find_from_candidates<'a, 'b, UI, UB, UL, VI, VB, VL>(
		candidates: std::collections::btree_map::Iter<&'a UB, BTreeSet<&'b VB>>,
		sigma: BTreeBijection<'a, 'b, UB, VB>,
		a: &BTreeMap<&'a UB, BlankSignature<'a, Self>>,
		b: &BTreeMap<&'b VB, BlankSignature<'b, V>>,
	) -> Option<BTreeBijection<'a, 'b, UB, VB>>
	where
		Self::Subject: AsRdfTerm<UI, UB, UL>,
		Self::Predicate: AsRdfTerm<UI, UB, UL>,
		Self::Object: AsRdfTerm<UI, UB, UL>,
		Self::GraphLabel: AsRdfTerm<UI, UB, UL>,
		V::Subject: AsRdfTerm<VI, VB, VL>,
		V::Predicate: AsRdfTerm<VI, VB, VL>,
		V::Object: AsRdfTerm<VI, VB, VL>,
		V::GraphLabel: AsRdfTerm<VI, VB, VL>,
		UI: PartialEq<VI>,
		UL: PartialEq<VL>,
		UB: Ord,
		VB: Ord;
}

impl<U: Dataset, V: Dataset> FindBTreeBlankIdBijection<V> for U {
	fn blank_quad_matches<'u, 'v, UI, UB, UL, VI, VB, VL>(
		a: Quad<&'u U::Subject, &'u U::Predicate, &'u U::Object, &'u U::GraphLabel>,
		b: Quad<&'v V::Subject, &'v V::Predicate, &'v V::Object, &'v V::GraphLabel>,
	) -> bool
	where
		U::Subject: AsRdfTerm<UI, UB, UL>,
		U::Predicate: AsRdfTerm<UI, UB, UL>,
		U::Object: AsRdfTerm<UI, UB, UL>,
		U::GraphLabel: AsRdfTerm<UI, UB, UL>,
		V::Subject: AsRdfTerm<VI, VB, VL>,
		V::Predicate: AsRdfTerm<VI, VB, VL>,
		V::Object: AsRdfTerm<VI, VB, VL>,
		V::GraphLabel: AsRdfTerm<VI, VB, VL>,
		UI: PartialEq<VI>,
		UL: PartialEq<VL>,
	{
		blank_term_matches(a.subject().as_rdf_term(), b.subject().as_rdf_term())
			&& blank_term_matches(a.predicate().as_rdf_term(), b.predicate().as_rdf_term())
			&& blank_term_matches(a.object().as_rdf_term(), b.object().as_rdf_term())
			&& match (a.graph(), b.graph()) {
				(Some(a), Some(b)) => blank_term_matches(a.as_rdf_term(), b.as_rdf_term()),
				(None, None) => true,
				_ => false,
			}
	}

	fn blank_quad_matches_with<'u, 'v, UI, UB, UL, VI, VB, VL>(
		sigma: &BTreeBijection<'u, 'v, UB, VB>,
		a: Quad<&'u U::Subject, &'u U::Predicate, &'u U::Object, &'u U::GraphLabel>,
		b: Quad<&'v V::Subject, &'v V::Predicate, &'v V::Object, &'v V::GraphLabel>,
	) -> bool
	where
		U::Subject: AsRdfTerm<UI, UB, UL>,
		U::Predicate: AsRdfTerm<UI, UB, UL>,
		U::Object: AsRdfTerm<UI, UB, UL>,
		U::GraphLabel: AsRdfTerm<UI, UB, UL>,
		V::Subject: AsRdfTerm<VI, VB, VL>,
		V::Predicate: AsRdfTerm<VI, VB, VL>,
		V::Object: AsRdfTerm<VI, VB, VL>,
		V::GraphLabel: AsRdfTerm<VI, VB, VL>,
		UI: PartialEq<VI>,
		UL: PartialEq<VL>,
		UB: Ord,
		VB: Ord,
	{
		blank_term_matches_with(sigma, a.subject().as_rdf_term(), b.subject().as_rdf_term())
			&& blank_term_matches_with(
				sigma,
				a.predicate().as_rdf_term(),
				b.predicate().as_rdf_term(),
			) && blank_term_matches_with(sigma, a.object().as_rdf_term(), b.object().as_rdf_term())
			&& match (a.graph(), b.graph()) {
				(Some(a), Some(b)) => {
					blank_term_matches_with(sigma, a.as_rdf_term(), b.as_rdf_term())
				}
				(None, None) => true,
				_ => false,
			}
	}

	fn find_from_candidates<'u, 'v, UI, UB, UL, VI, VB, VL>(
		mut candidates: std::collections::btree_map::Iter<&'u UB, BTreeSet<&'v VB>>,
		sigma: BTreeBijection<'u, 'v, UB, VB>,
		a: &BTreeMap<&'u UB, BlankSignature<'u, U>>,
		b: &BTreeMap<&'v VB, BlankSignature<'v, V>>,
	) -> Option<BTreeBijection<'u, 'v, UB, VB>>
	where
		U: FindBTreeBlankIdBijection<V>,
		U::Subject: AsRdfTerm<UI, UB, UL>,
		U::Predicate: AsRdfTerm<UI, UB, UL>,
		U::Object: AsRdfTerm<UI, UB, UL>,
		U::GraphLabel: AsRdfTerm<UI, UB, UL>,
		V::Subject: AsRdfTerm<VI, VB, VL>,
		V::Predicate: AsRdfTerm<VI, VB, VL>,
		V::Object: AsRdfTerm<VI, VB, VL>,
		V::GraphLabel: AsRdfTerm<VI, VB, VL>,
		UI: PartialEq<VI>,
		UL: PartialEq<VL>,
		UB: Ord,
		VB: Ord,
	{
		match candidates.next() {
			Some((a_blank_id, b_candidates)) => {
				for b_candidate in b_candidates {
					if !sigma.backward.contains_key(b_candidate) {
						// eprintln!("analyzing candidate {} for {}", b_candidate, a_blank_id);

						let mut new_sigma = sigma.clone();
						new_sigma.insert(a_blank_id, b_candidate);
						if a.get(a_blank_id)
							.unwrap()
							.matches_with(&new_sigma, b.get(b_candidate).unwrap())
						{
							// eprintln!("this is a valid candidate. continuing.");
							if let Some(final_sigma) =
								Self::find_from_candidates(candidates.clone(), new_sigma, a, b)
							{
								return Some(final_sigma);
							}
							// eprintln!("it didn't work out in the end. next candidate for {}.", a_blank_id);
						}
					}
				}

				// eprintln!("no valid candidate for {}", a_blank_id);
				None
			}
			None => Some(sigma),
		}
	}

	fn find_btree_blank_id_bijection<'u, 'v, UI: 'u, UB, UL: 'u, VI: 'v, VB, VL: 'v>(
		a: &'u U,
		b: &'v V,
	) -> Option<BTreeBijection<'u, 'v, UB, VB>>
	where
		U::Subject: Ord + AsRdfTerm<UI, UB, UL>,
		U::Predicate: Ord + AsRdfTerm<UI, UB, UL>,
		U::Object: Ord + AsRdfTerm<UI, UB, UL>,
		U::GraphLabel: Ord + AsRdfTerm<UI, UB, UL>,
		V::Subject: Ord + AsRdfTerm<VI, VB, VL>,
		V::Predicate: Ord + AsRdfTerm<VI, VB, VL>,
		V::Object: Ord + AsRdfTerm<VI, VB, VL>,
		V::GraphLabel: Ord + AsRdfTerm<VI, VB, VL>,
		UI: PartialEq<VI>,
		UL: PartialEq<VL>,
		UB: Ord,
		VB: Ord,
	{
		fn add_blank_quad<'d, D: Dataset, B: Ord>(
			blanks: &mut BTreeMap<&'d B, BlankSignature<'d, D>>,
			blank_id: &'d B,
			quad: Quad<&'d D::Subject, &'d D::Predicate, &'d D::Object, &'d D::GraphLabel>,
		) where
			D::Subject: Ord,
			D::Predicate: Ord,
			D::Object: Ord,
			D::GraphLabel: Ord,
		{
			match blanks.entry(blank_id) {
				Entry::Vacant(entry) => {
					entry.insert(BlankSignature::new(quad));
				}
				Entry::Occupied(mut entry) => entry.get_mut().insert(quad),
			}
		}

		fn collect_signatures<'d, D: Dataset, I: 'd, B, L: 'd>(
			map: &mut BTreeMap<&'d B, BlankSignature<'d, D>>,
			ds: &'d D,
		) where
			D::Subject: Ord + AsRdfTerm<I, B, L>,
			D::Predicate: Ord + AsRdfTerm<I, B, L>,
			D::Object: Ord + AsRdfTerm<I, B, L>,
			D::GraphLabel: Ord + AsRdfTerm<I, B, L>,
			B: Ord,
		{
			for quad @ Quad(subject, predicate, object, graph) in ds.quads() {
				let subject = subject.as_rdf_term();
				let predicate = predicate.as_rdf_term();
				let object = object.as_rdf_term();

				if let Some(blank_id) = subject.as_blank() {
					add_blank_quad(map, blank_id, quad);
				}

				if let Some(blank_id) = predicate.as_blank() {
					add_blank_quad(map, blank_id, quad);
				}

				if let Some(blank_id) = object.as_blank() {
					add_blank_quad(map, blank_id, quad);
				}

				if let Some(graph) = graph {
					if let Some(blank_id) = graph.as_rdf_term().as_blank() {
						add_blank_quad(map, blank_id, quad);
					}
				}
			}
		}

		fn split_by_size<'s, 'd, D: Dataset, B: Ord>(
			blanks: &'s BTreeMap<&'d B, BlankSignature<'d, D>>,
		) -> BTreeMap<usize, BTreeMap<&'d B, &'s BlankSignature<'d, D>>> {
			let mut result = BTreeMap::new();

			for (&blank_id, sig) in blanks {
				match result.entry(sig.len()) {
					Entry::Vacant(entry) => {
						let mut map = BTreeMap::new();
						map.insert(blank_id, sig);
						entry.insert(map);
					}
					Entry::Occupied(mut entry) => {
						entry.get_mut().insert(blank_id, sig);
					}
				}
			}

			result
		}

		// Step 1: collect signatures.
		let mut a_blanks_map = BTreeMap::new();
		let mut b_blanks_map = BTreeMap::new();
		collect_signatures(&mut a_blanks_map, a);
		collect_signatures(&mut b_blanks_map, b);

		if a_blanks_map.len() == b_blanks_map.len() {
			// Step 2: split by sizes.
			let a_groups = split_by_size(&a_blanks_map);
			let b_groups = split_by_size(&b_blanks_map);

			if a_groups.len() == b_groups.len() {
				if a_groups.iter().all(|(len, _)| b_groups.contains_key(len)) {
					// Step 3: find candidates for each blank id.
					let mut candidates = BTreeMap::new();
					for (len, a_group) in a_groups {
						let b_group = b_groups.get(&len).unwrap();

						for (a_blank_id, a_sig) in a_group {
							let mut a_blank_id_candidates = BTreeSet::new();
							for (b_blank_id, b_sig) in b_group {
								if a_sig.matches(b_sig) {
									a_blank_id_candidates.insert(*b_blank_id);
								}
							}

							if a_blank_id_candidates.is_empty() {
								// eprintln!("no candidates found for {}", a_blank_id);
								return None;
							}

							candidates.insert(a_blank_id, a_blank_id_candidates);
						}
					}

					Self::find_from_candidates(
						candidates.iter(),
						BTreeBijection::new(),
						&a_blanks_map,
						&b_blanks_map,
					)
				} else {
					// eprintln!("different group lengths");
					None
				}
			} else {
				// eprintln!("different group count");
				None
			}
		} else {
			// eprintln!("different blank node count");
			None
		}
	}
}

/// Blank node identifier bijection
/// between two (isomorphic) datasets.
#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
pub struct BTreeBijection<'a, 'b, A: ?Sized = BlankIdBuf, B: ?Sized = BlankIdBuf> {
	pub forward: BTreeMap<&'a A, &'b B>,
	pub backward: BTreeMap<&'b B, &'a A>,
}

impl<'a, 'b, A: ?Sized, B: ?Sized> BTreeBijection<'a, 'b, A, B> {
	fn new() -> Self {
		Self {
			forward: BTreeMap::new(),
			backward: BTreeMap::new(),
		}
	}

	fn insert(&mut self, a: &'a A, b: &'b B)
	where
		A: Ord,
		B: Ord,
	{
		self.forward.insert(a, b);
		self.backward.insert(b, a);
	}
}

#[allow(clippy::type_complexity)]
pub(crate) struct BlankSignature<'a, A: ?Sized + Dataset>(
	BTreeSet<Quad<&'a A::Subject, &'a A::Predicate, &'a A::Object, &'a A::GraphLabel>>,
);

impl<'a, U: Dataset> BlankSignature<'a, U> {
	pub fn new(
		quad: Quad<&'a U::Subject, &'a U::Predicate, &'a U::Object, &'a U::GraphLabel>,
	) -> Self
	where
		U::Subject: Ord,
		U::Predicate: Ord,
		U::Object: Ord,
		U::GraphLabel: Ord,
	{
		Self(Some(quad).into_iter().collect())
	}

	pub fn insert(
		&mut self,
		quad: Quad<&'a U::Subject, &'a U::Predicate, &'a U::Object, &'a U::GraphLabel>,
	) where
		U::Subject: Ord,
		U::Predicate: Ord,
		U::Object: Ord,
		U::GraphLabel: Ord,
	{
		self.0.insert(quad);
	}

	pub fn len(&self) -> usize {
		self.0.len()
	}

	pub fn matches<V: Dataset, UI, UB, UL, VI, VB, VL>(&self, other: &BlankSignature<V>) -> bool
	where
		U: FindBTreeBlankIdBijection<V>,
		U::Subject: AsRdfTerm<UI, UB, UL>,
		U::Predicate: AsRdfTerm<UI, UB, UL>,
		U::Object: AsRdfTerm<UI, UB, UL>,
		U::GraphLabel: AsRdfTerm<UI, UB, UL>,
		V::Subject: AsRdfTerm<VI, VB, VL>,
		V::Predicate: AsRdfTerm<VI, VB, VL>,
		V::Object: AsRdfTerm<VI, VB, VL>,
		V::GraphLabel: AsRdfTerm<VI, VB, VL>,
		UI: PartialEq<VI>,
		UL: PartialEq<VL>,
		UB: Ord,
		VB: Ord,
	{
		if self.len() == other.len() {
			let mut other: Vec<_> = other.0.iter().map(|q| Some(*q)).collect();
			'next_quad: for quad in &self.0 {
				for other_quad in &mut other {
					if let Some(oq) = other_quad {
						if U::blank_quad_matches(*quad, *oq) {
							other_quad.take();
							continue 'next_quad;
						}
					}
				}

				return false;
			}

			true
		} else {
			false
		}
	}

	pub fn matches_with<'b, V: Dataset, UI, UB, UL, VI, VB, VL>(
		&self,
		sigma: &BTreeBijection<'a, 'b, UB, VB>,
		other: &BlankSignature<'b, V>,
	) -> bool
	where
		U: FindBTreeBlankIdBijection<V>,
		U::Subject: AsRdfTerm<UI, UB, UL>,
		U::Predicate: AsRdfTerm<UI, UB, UL>,
		U::Object: AsRdfTerm<UI, UB, UL>,
		U::GraphLabel: AsRdfTerm<UI, UB, UL>,
		V::Subject: AsRdfTerm<VI, VB, VL>,
		V::Predicate: AsRdfTerm<VI, VB, VL>,
		V::Object: AsRdfTerm<VI, VB, VL>,
		V::GraphLabel: AsRdfTerm<VI, VB, VL>,
		UI: PartialEq<VI>,
		UL: PartialEq<VL>,
		UB: Ord,
		VB: Ord,
	{
		if self.len() == other.len() {
			let mut other: Vec<_> = other.0.iter().map(|q| Some(*q)).collect();
			'next_quad: for quad in &self.0 {
				for other_quad in &mut other {
					if let Some(oq) = other_quad {
						if U::blank_quad_matches_with(sigma, *quad, *oq) {
							// eprintln!("matching {} with {}", quad, oq);
							other_quad.take();
							continue 'next_quad;
						}
					}
				}

				// eprintln!("could not match {} with anything", quad);
				return false;
			}

			true
		} else {
			false
		}
	}
}

fn blank_term_matches_with<'u, 'v, UI, UB, UL, VI, VB, VL>(
	sigma: &BTreeBijection<'u, 'v, UB, VB>,
	a: rdf_types::Term<Id<&'u UI, &'u UB>, &'u UL>,
	b: rdf_types::Term<Id<&'v VI, &'v VB>, &'v VL>,
) -> bool
where
	UI: PartialEq<VI>,
	UL: PartialEq<VL>,
	UB: Ord,
	VB: Ord,
{
	let r = match (a, b) {
		(rdf_types::Term::Id(Id::Blank(a)), rdf_types::Term::Id(Id::Blank(b))) => {
			match sigma.forward.get(a) {
				Some(&c) => c == b,
				None => match sigma.backward.get(b) {
					Some(&c) => a == c,
					None => true,
				},
			}
		}
		(rdf_types::Term::Id(Id::Iri(a)), rdf_types::Term::Id(Id::Iri(b))) => a == b,
		(rdf_types::Term::Literal(a), rdf_types::Term::Literal(b)) => a == b,
		_ => false,
	};

	r
}
