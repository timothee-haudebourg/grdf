pub(crate) mod btree;
pub(crate) mod hash;

pub use btree::BTreeBijection;
pub use hash::HashBijection;

fn blank_term_matches<'a, 'b, AI, AB, AL, BI, BB, BL>(
	a: rdf_types::Term<&'a AI, &'a AB, &'a AL>,
	b: rdf_types::Term<&'b BI, &'b BB, &'b BL>,
) -> bool
where
	AI: PartialEq<BI>,
	AL: PartialEq<BL>,
{
	match (a, b) {
		(rdf_types::Term::Blank(_), rdf_types::Term::Blank(_)) => true,
		(rdf_types::Term::Iri(a), rdf_types::Term::Iri(b)) => a == b,
		(rdf_types::Term::Literal(a), rdf_types::Term::Literal(b)) => a == b,
		_ => false,
	}
}

#[cfg(test)]
mod tests {
	use crate::HashDataset;
	use rdf_types::{BlankId, BlankIdBuf, Quad, Term};
	use static_iref::iri;

	#[test]
	fn substitution_01() {
		let mut a: HashDataset = HashDataset::new();
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

		let sigma = a.find_blank_id_bijection(&b).unwrap();
		assert_eq!(
			sigma.forward.get(BlankId::new("_:0").unwrap()).cloned(),
			Some(&BlankIdBuf::new("_:1".to_owned()).unwrap())
		)
	}

	#[test]
	#[should_panic]
	fn substitution_02() {
		let mut a: HashDataset = HashDataset::new();
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

		let sigma = a.find_blank_id_bijection(&b).unwrap();
		assert_eq!(
			sigma.forward.get(BlankId::new("_:0").unwrap()).cloned(),
			Some(&BlankIdBuf::new("_:1".to_owned()).unwrap())
		)
	}

	#[test]
	#[should_panic]
	fn substitution_03() {
		let mut a: HashDataset = HashDataset::new();
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

		let sigma = a.find_blank_id_bijection(&b).unwrap();
		assert_eq!(
			sigma.forward.get(BlankId::new("_:0").unwrap()).cloned(),
			Some(&BlankIdBuf::new("_:1".to_owned()).unwrap())
		)
	}

	#[test]
	#[should_panic]
	fn substitution_04() {
		let mut a: HashDataset = HashDataset::new();
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

		let sigma = a.find_blank_id_bijection(&b).unwrap();
		assert_eq!(
			sigma.forward.get(BlankId::new("_:0").unwrap()).cloned(),
			Some(&BlankIdBuf::new("_:1".to_owned()).unwrap())
		)
	}

	#[test]
	fn substitution_05() {
		let mut a: HashDataset = HashDataset::new();
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

		let sigma = a.find_blank_id_bijection(&b).unwrap();
		assert_eq!(
			sigma.forward.get(BlankId::new("_:0").unwrap()).cloned(),
			Some(&BlankIdBuf::new("_:10".to_owned()).unwrap())
		);
		assert_eq!(
			sigma.forward.get(BlankId::new("_:1").unwrap()).cloned(),
			Some(&BlankIdBuf::new("_:11".to_owned()).unwrap())
		);
		assert_eq!(
			sigma.forward.get(BlankId::new("_:2").unwrap()).cloned(),
			Some(&BlankIdBuf::new("_:12".to_owned()).unwrap())
		)
	}
}
