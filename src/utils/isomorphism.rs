use crate::{Dataset, Quad};
use rdf_types::BlankId;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};

/// Blank node identifier bijection
/// between two (isomorphic) datasets.
#[derive(Clone)]
pub struct BlankIdBijection<'a, 'b> {
	pub forward: HashMap<&'a BlankId, &'b BlankId>,
	pub backward: HashMap<&'b BlankId, &'a BlankId>,
}

impl<'a, 'b> BlankIdBijection<'a, 'b> {
	fn new() -> Self {
		Self {
			forward: HashMap::new(),
			backward: HashMap::new(),
		}
	}

	fn insert(&mut self, a: &'a BlankId, b: &'b BlankId) {
		self.forward.insert(a, b);
		self.backward.insert(b, a);
	}
}

type QuadRef<'a> =
	Quad<&'a rdf_types::Term, &'a rdf_types::Term, &'a rdf_types::Term, &'a rdf_types::Term>;

struct BlankSignature<'a>(HashSet<QuadRef<'a>>);

impl<'a> BlankSignature<'a> {
	pub fn new(quad: QuadRef<'a>) -> Self {
		Self(Some(quad).into_iter().collect())
	}

	pub fn insert(&mut self, quad: QuadRef<'a>) {
		self.0.insert(quad);
	}

	pub fn len(&self) -> usize {
		self.0.len()
	}

	pub fn matches<'b>(&self, other: &BlankSignature<'b>) -> bool {
		if self.len() == other.len() {
			let mut other: Vec<_> = other.0.iter().map(|q| Some(*q)).collect();
			'next_quad: for quad in &self.0 {
				for other_quad in &mut other {
					if let Some(oq) = other_quad {
						if blank_quad_matches(*quad, *oq) {
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

	pub fn matches_with<'b>(
		&self,
		sigma: &BlankIdBijection<'a, 'b>,
		other: &BlankSignature<'b>,
	) -> bool {
		if self.len() == other.len() {
			let mut other: Vec<_> = other.0.iter().map(|q| Some(*q)).collect();
			'next_quad: for quad in &self.0 {
				for other_quad in &mut other {
					if let Some(oq) = other_quad {
						if blank_quad_matches_with(sigma, *quad, *oq) {
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

fn blank_term_matches(a: &rdf_types::Term, b: &rdf_types::Term) -> bool {
	match (a, b) {
		(rdf_types::Term::Blank(_), rdf_types::Term::Blank(_)) => true,
		(a, b) => a == b,
	}
}

fn blank_quad_matches(a: QuadRef, b: QuadRef) -> bool {
	blank_term_matches(a.subject(), b.subject())
		&& blank_term_matches(a.predicate(), b.predicate())
		&& blank_term_matches(a.object(), b.object())
		&& match (a.graph(), b.graph()) {
			(Some(a), Some(b)) => blank_term_matches(a, b),
			(None, None) => true,
			_ => false,
		}
}

fn blank_term_matches_with<'a, 'b>(
	sigma: &BlankIdBijection<'a, 'b>,
	a: &'a rdf_types::Term,
	b: &'b rdf_types::Term,
) -> bool {
	let r = match (a, b) {
		(rdf_types::Term::Blank(a), rdf_types::Term::Blank(b)) => {
			match sigma.forward.get(a.as_ref()) {
				Some(&c) => c == b.as_ref(),
				None => match sigma.backward.get(b.as_ref()) {
					Some(&c) => a.as_ref() == c,
					None => true,
				},
			}
		}
		(a, b) => a == b,
	};

	r
}

fn blank_quad_matches_with<'a, 'b>(
	sigma: &BlankIdBijection<'a, 'b>,
	a: QuadRef<'a>,
	b: QuadRef<'b>,
) -> bool {
	blank_term_matches_with(sigma, a.subject(), b.subject())
		&& blank_term_matches_with(sigma, a.predicate(), b.predicate())
		&& blank_term_matches_with(sigma, a.object(), b.object())
		&& match (a.graph(), b.graph()) {
			(Some(a), Some(b)) => blank_term_matches_with(sigma, a, b),
			(None, None) => true,
			_ => false,
		}
}

/// Find a blank node identifier substitution from the blank no identifiers of `a` to the blank node identifiers of `b`
/// so that the sub-dataset of `a` containing all the quads with blank node identifier
/// is equal to the sub-dataset of `b` containing all the quads with a blank node identifier
/// once the substitution if applied.
///
/// The function ignores all the quads having no blank node identifiers.
pub fn find_blank_id_bijection<'a, 'b, A: Dataset, B: Dataset>(
	a: &'a A,
	b: &'b B,
) -> Option<BlankIdBijection<'a, 'b>> {
	fn add_blank_quad<'d>(
		blanks: &mut HashMap<&'d BlankId, BlankSignature<'d>>,
		blank_id: &'d BlankId,
		quad: QuadRef<'d>,
	) {
		match blanks.entry(blank_id) {
			Entry::Vacant(entry) => {
				entry.insert(BlankSignature::new(quad));
			}
			Entry::Occupied(mut entry) => entry.get_mut().insert(quad),
		}
	}

	fn collect_signatures<'d, D: Dataset>(
		map: &mut HashMap<&'d BlankId, BlankSignature<'d>>,
		ds: &'d D,
	) {
		for quad @ Quad(subject, predicate, object, graph) in ds.quads() {
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
				if let Some(blank_id) = graph.as_blank() {
					add_blank_quad(map, blank_id, quad);
				}
			}
		}
	}

	fn split_by_size<'s, 'd>(
		blanks: &'s HashMap<&'d BlankId, BlankSignature<'d>>,
	) -> HashMap<usize, HashMap<&'d BlankId, &'s BlankSignature<'d>>> {
		let mut result = HashMap::new();

		for (&blank_id, sig) in blanks {
			match result.entry(sig.len()) {
				Entry::Vacant(entry) => {
					let mut map = HashMap::new();
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

	fn find_from_candidates<'a, 'b>(
		mut candidates: std::collections::hash_map::Iter<&'a BlankId, HashSet<&'b BlankId>>,
		sigma: BlankIdBijection<'a, 'b>,
		a: &HashMap<&'a BlankId, BlankSignature<'a>>,
		b: &HashMap<&'b BlankId, BlankSignature<'b>>,
	) -> Option<BlankIdBijection<'a, 'b>> {
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
								find_from_candidates(candidates.clone(), new_sigma, a, b)
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

	// Step 1: collect signatures.
	let mut a_blanks_map = HashMap::new();
	let mut b_blanks_map = HashMap::new();
	collect_signatures(&mut a_blanks_map, a);
	collect_signatures(&mut b_blanks_map, b);

	if a_blanks_map.len() == b_blanks_map.len() {
		// Step 2: split by sizes.
		let a_groups = split_by_size(&a_blanks_map);
		let b_groups = split_by_size(&b_blanks_map);

		if a_groups.len() == b_groups.len() {
			if a_groups.iter().all(|(len, _)| b_groups.contains_key(len)) {
				// Step 3: find candidates for each blank id.
				let mut candidates = HashMap::new();
				for (len, a_group) in a_groups {
					let b_group = b_groups.get(&len).unwrap();

					for (a_blank_id, a_sig) in a_group {
						let mut a_blank_id_candidates = HashSet::new();
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

				find_from_candidates(
					candidates.iter(),
					BlankIdBijection::new(),
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

#[cfg(test)]
mod tests {
	use super::*;
	use crate::{HashDataset, MutableDataset};
	use rdf_types::{BlankIdBuf, Term};
	use static_iref::iri;

	#[test]
	fn substitution_01() {
		let mut a = HashDataset::new();
		let mut b = HashDataset::new();

		a.insert(Quad(
			Term::Blank(BlankIdBuf::from_u8(0)),
			Term::Iri(iri!("http://example.com/0").into()),
			Term::Iri(iri!("http://example.com/1").into()),
			None,
		));

		b.insert(Quad(
			Term::Blank(BlankIdBuf::from_u8(1)),
			Term::Iri(iri!("http://example.com/0").into()),
			Term::Iri(iri!("http://example.com/1").into()),
			None,
		));

		let sigma = find_blank_id_bijection(&a, &b).unwrap();
		assert_eq!(
			sigma.forward.get(BlankId::new("_:0").unwrap()).cloned(),
			Some(BlankId::new("_:1").unwrap())
		)
	}

	#[test]
	#[should_panic]
	fn substitution_02() {
		let mut a = HashDataset::new();
		let mut b = HashDataset::new();

		a.insert(Quad(
			Term::Blank(BlankIdBuf::from_u8(0)),
			Term::Iri(iri!("http://example.com/0").into()),
			Term::Iri(iri!("http://example.com/1").into()),
			None,
		));

		b.insert(Quad(
			Term::Blank(BlankIdBuf::from_u8(1)),
			Term::Iri(iri!("http://example.com/0").into()),
			Term::Iri(iri!("http://example.com/2").into()),
			None,
		));

		let sigma = find_blank_id_bijection(&a, &b).unwrap();
		assert_eq!(
			sigma.forward.get(BlankId::new("_:0").unwrap()).cloned(),
			Some(BlankId::new("_:1").unwrap())
		)
	}

	#[test]
	#[should_panic]
	fn substitution_03() {
		let mut a = HashDataset::new();
		let mut b = HashDataset::new();

		a.insert(Quad(
			Term::Blank(BlankIdBuf::from_u8(0)),
			Term::Iri(iri!("http://example.com/0").into()),
			Term::Iri(iri!("http://example.com/1").into()),
			None,
		));

		b.insert(Quad(
			Term::Blank(BlankIdBuf::from_u8(1)),
			Term::Iri(iri!("http://example.com/2").into()),
			Term::Iri(iri!("http://example.com/1").into()),
			None,
		));

		let sigma = find_blank_id_bijection(&a, &b).unwrap();
		assert_eq!(
			sigma.forward.get(BlankId::new("_:0").unwrap()).cloned(),
			Some(BlankId::new("_:1").unwrap())
		)
	}

	#[test]
	#[should_panic]
	fn substitution_04() {
		let mut a = HashDataset::new();
		let mut b = HashDataset::new();

		a.insert(Quad(
			Term::Blank(BlankIdBuf::from_u8(0)),
			Term::Iri(iri!("http://example.com/0").into()),
			Term::Iri(iri!("http://example.com/1").into()),
			None,
		));

		b.insert(Quad(
			Term::Blank(BlankIdBuf::from_u8(1)),
			Term::Iri(iri!("http://example.com/0").into()),
			Term::Iri(iri!("http://example.com/1").into()),
			Some(Term::Iri(iri!("http://example.com/2").into())),
		));

		let sigma = find_blank_id_bijection(&a, &b).unwrap();
		assert_eq!(
			sigma.forward.get(BlankId::new("_:0").unwrap()).cloned(),
			Some(BlankId::new("_:1").unwrap())
		)
	}

	#[test]
	fn substitution_05() {
		let mut a = HashDataset::new();
		let mut b = HashDataset::new();

		a.insert(Quad(
			Term::Blank(BlankIdBuf::from_u8(0)),
			Term::Iri(iri!("http://example.com/0").into()),
			Term::Iri(iri!("http://example.com/1").into()),
			None,
		));
		a.insert(Quad(
			Term::Blank(BlankIdBuf::from_u8(1)),
			Term::Blank(BlankIdBuf::from_u8(0)),
			Term::Iri(iri!("http://example.com/1").into()),
			None,
		));
		a.insert(Quad(
			Term::Blank(BlankIdBuf::from_u8(2)),
			Term::Blank(BlankIdBuf::from_u8(0)),
			Term::Iri(iri!("http://example.com/2").into()),
			None,
		));

		b.insert(Quad(
			Term::Blank(BlankIdBuf::from_u8(10)),
			Term::Iri(iri!("http://example.com/0").into()),
			Term::Iri(iri!("http://example.com/1").into()),
			None,
		));
		b.insert(Quad(
			Term::Blank(BlankIdBuf::from_u8(11)),
			Term::Blank(BlankIdBuf::from_u8(10)),
			Term::Iri(iri!("http://example.com/1").into()),
			None,
		));
		b.insert(Quad(
			Term::Blank(BlankIdBuf::from_u8(12)),
			Term::Blank(BlankIdBuf::from_u8(10)),
			Term::Iri(iri!("http://example.com/2").into()),
			None,
		));

		let sigma = find_blank_id_bijection(&a, &b).unwrap();
		assert_eq!(
			sigma.forward.get(BlankId::new("_:0").unwrap()).cloned(),
			Some(BlankId::new("_:10").unwrap())
		);
		assert_eq!(
			sigma.forward.get(BlankId::new("_:1").unwrap()).cloned(),
			Some(BlankId::new("_:11").unwrap())
		);
		assert_eq!(
			sigma.forward.get(BlankId::new("_:2").unwrap()).cloned(),
			Some(BlankId::new("_:12").unwrap())
		)
	}
}
