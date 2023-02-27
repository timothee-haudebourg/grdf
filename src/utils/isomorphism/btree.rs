use super::blank_term_matches;
use crate::{Dataset, Quad};
use derivative::Derivative;
use rdf_types::{AsTerm, BlankIdBuf, Id};
use std::collections::btree_map::Entry;
use std::collections::{BTreeMap, BTreeSet};

pub(crate) trait FindBTreeBlankIdBijection<B: Dataset>: Dataset
where
	Self::Subject: AsTerm,
	B::Subject: AsTerm,
{
	fn blank_quad_matches<'a, 'b>(
		a: Quad<&'a Self::Subject, &'a Self::Predicate, &'a Self::Object, &'a Self::GraphLabel>,
		b: Quad<&'b B::Subject, &'b B::Predicate, &'b B::Object, &'b B::GraphLabel>,
	) -> bool;

	fn blank_quad_matches_with<'a, 'b>(
		sigma: &BTreeBijection<
			'a,
			'b,
			<Self::Subject as AsTerm>::BlankId,
			<B::Subject as AsTerm>::BlankId,
		>,
		a: Quad<&'a Self::Subject, &'a Self::Predicate, &'a Self::Object, &'a Self::GraphLabel>,
		b: Quad<&'b B::Subject, &'b B::Predicate, &'b B::Object, &'b B::GraphLabel>,
	) -> bool;

	/// Find a blank node identifier substitution from the blank no identifiers of `a` to the blank node identifiers of `b`
	/// so that the sub-dataset of `a` containing all the quads with blank node identifier
	/// is equal to the sub-dataset of `b` containing all the quads with a blank node identifier
	/// once the substitution if applied.
	///
	/// The function ignores all the quads having no blank node identifiers.
	#[allow(clippy::type_complexity)]
	fn find_btree_blank_id_bijection<'a, 'b>(
		a: &'a Self,
		b: &'b B,
	) -> Option<
		BTreeBijection<'a, 'b, <Self::Subject as AsTerm>::BlankId, <B::Subject as AsTerm>::BlankId>,
	>;

	#[allow(clippy::type_complexity)]
	fn find_from_candidates<'a, 'b>(
		candidates: std::collections::btree_map::Iter<
			&'a <Self::Subject as AsTerm>::BlankId,
			BTreeSet<&'b <B::Subject as AsTerm>::BlankId>,
		>,
		sigma: BTreeBijection<
			'a,
			'b,
			<Self::Subject as AsTerm>::BlankId,
			<B::Subject as AsTerm>::BlankId,
		>,
		a: &BTreeMap<&'a <Self::Subject as AsTerm>::BlankId, BlankSignature<'a, Self>>,
		b: &BTreeMap<&'b <B::Subject as AsTerm>::BlankId, BlankSignature<'b, B>>,
	) -> Option<
		BTreeBijection<'a, 'b, <Self::Subject as AsTerm>::BlankId, <B::Subject as AsTerm>::BlankId>,
	>;
}

impl<A: Dataset, B: Dataset> FindBTreeBlankIdBijection<B> for A
where
	A::Subject: Ord + AsTerm,
	B::Subject: Ord + AsTerm,
	<A::Subject as AsTerm>::Iri: PartialEq<<B::Subject as AsTerm>::Iri>,
	<A::Subject as AsTerm>::Literal: PartialEq<<B::Subject as AsTerm>::Literal>,
	<A::Subject as AsTerm>::BlankId: Ord,
	<B::Subject as AsTerm>::BlankId: Ord,
	A::Predicate: Ord + AsTerm<BlankId = <A::Subject as AsTerm>::BlankId>,
	B::Predicate: Ord + AsTerm<BlankId = <B::Subject as AsTerm>::BlankId>,
	<A::Predicate as AsTerm>::Iri: PartialEq<<B::Predicate as AsTerm>::Iri>,
	<A::Predicate as AsTerm>::Literal: PartialEq<<B::Predicate as AsTerm>::Literal>,
	A::Object: Ord + AsTerm<BlankId = <A::Subject as AsTerm>::BlankId>,
	B::Object: Ord + AsTerm<BlankId = <B::Subject as AsTerm>::BlankId>,
	<A::Object as AsTerm>::Iri: PartialEq<<B::Object as AsTerm>::Iri>,
	<A::Object as AsTerm>::Literal: PartialEq<<B::Object as AsTerm>::Literal>,
	A::GraphLabel: Ord + AsTerm<BlankId = <A::Subject as AsTerm>::BlankId>,
	B::GraphLabel: Ord + AsTerm<BlankId = <B::Subject as AsTerm>::BlankId>,
	<A::GraphLabel as AsTerm>::Iri: PartialEq<<B::GraphLabel as AsTerm>::Iri>,
	<A::GraphLabel as AsTerm>::Literal: PartialEq<<B::GraphLabel as AsTerm>::Literal>,
{
	fn blank_quad_matches<'a, 'b>(
		a: Quad<&'a A::Subject, &'a A::Predicate, &'a A::Object, &'a A::GraphLabel>,
		b: Quad<&'b B::Subject, &'b B::Predicate, &'b B::Object, &'b B::GraphLabel>,
	) -> bool {
		blank_term_matches(a.subject().as_term(), b.subject().as_term())
			&& blank_term_matches(a.predicate().as_term(), b.predicate().as_term())
			&& blank_term_matches(a.object().as_term(), b.object().as_term())
			&& match (a.graph(), b.graph()) {
				(Some(a), Some(b)) => blank_term_matches(a.as_term(), b.as_term()),
				(None, None) => true,
				_ => false,
			}
	}

	fn blank_quad_matches_with<'a, 'b>(
		sigma: &BTreeBijection<
			'a,
			'b,
			<A::Subject as AsTerm>::BlankId,
			<B::Subject as AsTerm>::BlankId,
		>,
		a: Quad<&'a A::Subject, &'a A::Predicate, &'a A::Object, &'a A::GraphLabel>,
		b: Quad<&'b B::Subject, &'b B::Predicate, &'b B::Object, &'b B::GraphLabel>,
	) -> bool {
		blank_term_matches_with(sigma, a.subject().as_term(), b.subject().as_term())
			&& blank_term_matches_with(sigma, a.predicate().as_term(), b.predicate().as_term())
			&& blank_term_matches_with(sigma, a.object().as_term(), b.object().as_term())
			&& match (a.graph(), b.graph()) {
				(Some(a), Some(b)) => blank_term_matches_with(sigma, a.as_term(), b.as_term()),
				(None, None) => true,
				_ => false,
			}
	}

	fn find_from_candidates<'a, 'b>(
		mut candidates: std::collections::btree_map::Iter<
			&'a <A::Subject as AsTerm>::BlankId,
			BTreeSet<&'b <B::Subject as AsTerm>::BlankId>,
		>,
		sigma: BTreeBijection<
			'a,
			'b,
			<A::Subject as AsTerm>::BlankId,
			<B::Subject as AsTerm>::BlankId,
		>,
		a: &BTreeMap<&'a <A::Subject as AsTerm>::BlankId, BlankSignature<'a, A>>,
		b: &BTreeMap<&'b <B::Subject as AsTerm>::BlankId, BlankSignature<'b, B>>,
	) -> Option<
		BTreeBijection<'a, 'b, <A::Subject as AsTerm>::BlankId, <B::Subject as AsTerm>::BlankId>,
	> {
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

	fn find_btree_blank_id_bijection<'a, 'b>(
		a: &'a A,
		b: &'b B,
	) -> Option<
		BTreeBijection<'a, 'b, <A::Subject as AsTerm>::BlankId, <B::Subject as AsTerm>::BlankId>,
	> {
		fn add_blank_quad<'d, D: Dataset>(
			blanks: &mut BTreeMap<&'d <D::Subject as AsTerm>::BlankId, BlankSignature<'d, D>>,
			blank_id: &'d <D::Subject as AsTerm>::BlankId,
			quad: Quad<&'d D::Subject, &'d D::Predicate, &'d D::Object, &'d D::GraphLabel>,
		) where
			D::Subject: Ord + AsTerm,
			D::Predicate: Ord + AsTerm<BlankId = <D::Subject as AsTerm>::BlankId>,
			D::Object: Ord + AsTerm<BlankId = <D::Subject as AsTerm>::BlankId>,
			D::GraphLabel: Ord + AsTerm<BlankId = <D::Subject as AsTerm>::BlankId>,
			<D::Subject as AsTerm>::BlankId: Ord,
		{
			match blanks.entry(blank_id) {
				Entry::Vacant(entry) => {
					entry.insert(BlankSignature::new(quad));
				}
				Entry::Occupied(mut entry) => entry.get_mut().insert(quad),
			}
		}

		fn collect_signatures<'d, D: Dataset>(
			map: &mut BTreeMap<&'d <D::Subject as AsTerm>::BlankId, BlankSignature<'d, D>>,
			ds: &'d D,
		) where
			D::Subject: Ord + AsTerm,
			D::Predicate: Ord + AsTerm<BlankId = <D::Subject as AsTerm>::BlankId>,
			D::Object: Ord + AsTerm<BlankId = <D::Subject as AsTerm>::BlankId>,
			D::GraphLabel: Ord + AsTerm<BlankId = <D::Subject as AsTerm>::BlankId>,
			<D::Subject as AsTerm>::BlankId: Ord,
		{
			for quad @ Quad(subject, predicate, object, graph) in ds.quads() {
				let subject = subject.as_term();
				let predicate = predicate.as_term();
				let object = object.as_term();

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
					if let Some(blank_id) = graph.as_term().as_blank() {
						add_blank_quad(map, blank_id, quad);
					}
				}
			}
		}

		fn split_by_size<'s, 'd, D: Dataset>(
			blanks: &'s BTreeMap<&'d <D::Subject as AsTerm>::BlankId, BlankSignature<'d, D>>,
		) -> BTreeMap<usize, BTreeMap<&'d <D::Subject as AsTerm>::BlankId, &'s BlankSignature<'d, D>>>
		where
			D::Subject: AsTerm,
			D::Predicate: AsTerm<BlankId = <D::Subject as AsTerm>::BlankId>,
			D::Object: AsTerm<BlankId = <D::Subject as AsTerm>::BlankId>,
			D::GraphLabel: AsTerm<BlankId = <D::Subject as AsTerm>::BlankId>,
			<D::Subject as AsTerm>::BlankId: Ord,
		{
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

impl<'a, A: Dataset> BlankSignature<'a, A> {
	pub fn new(
		quad: Quad<&'a A::Subject, &'a A::Predicate, &'a A::Object, &'a A::GraphLabel>,
	) -> Self
	where
		A::Subject: Ord,
		A::Predicate: Ord,
		A::Object: Ord,
		A::GraphLabel: Ord,
	{
		Self(Some(quad).into_iter().collect())
	}

	pub fn insert(
		&mut self,
		quad: Quad<&'a A::Subject, &'a A::Predicate, &'a A::Object, &'a A::GraphLabel>,
	) where
		A::Subject: Ord,
		A::Predicate: Ord,
		A::Object: Ord,
		A::GraphLabel: Ord,
	{
		self.0.insert(quad);
	}

	pub fn len(&self) -> usize {
		self.0.len()
	}

	pub fn matches<B: Dataset>(&self, other: &BlankSignature<B>) -> bool
	where
		A::Subject: AsTerm,
		B::Subject: AsTerm,
		A: FindBTreeBlankIdBijection<B>,
	{
		if self.len() == other.len() {
			let mut other: Vec<_> = other.0.iter().map(|q| Some(*q)).collect();
			'next_quad: for quad in &self.0 {
				for other_quad in &mut other {
					if let Some(oq) = other_quad {
						if A::blank_quad_matches(*quad, *oq) {
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

	pub fn matches_with<'b, B: Dataset>(
		&self,
		sigma: &BTreeBijection<
			'a,
			'b,
			<A::Subject as AsTerm>::BlankId,
			<B::Subject as AsTerm>::BlankId,
		>,
		other: &BlankSignature<'b, B>,
	) -> bool
	where
		A: FindBTreeBlankIdBijection<B>,
		A::Subject: AsTerm,
		B::Subject: AsTerm,
	{
		if self.len() == other.len() {
			let mut other: Vec<_> = other.0.iter().map(|q| Some(*q)).collect();
			'next_quad: for quad in &self.0 {
				for other_quad in &mut other {
					if let Some(oq) = other_quad {
						if A::blank_quad_matches_with(sigma, *quad, *oq) {
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

fn blank_term_matches_with<'a, 'b, AI, AB, AL, BI, BB, BL>(
	sigma: &BTreeBijection<'a, 'b, AB, BB>,
	a: rdf_types::Term<&'a AI, &'a AB, &'a AL>,
	b: rdf_types::Term<&'b BI, &'b BB, &'b BL>,
) -> bool
where
	AI: PartialEq<BI>,
	AL: PartialEq<BL>,
	AB: Ord,
	BB: Ord,
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
