pub(crate) mod btree;
pub(crate) mod hash;

pub use btree::BTreeBijection;
pub use hash::HashBijection;
use rdf_types::Id;

fn blank_term_matches<'u, 'v, UI, UB, UL, VI, VB, VL>(
	a: rdf_types::Term<Id<&'u UI, &'u UB>, &'u UL>,
	b: rdf_types::Term<Id<&'v VI, &'v VB>, &'v VL>,
) -> bool
where
	UI: PartialEq<VI>,
	UL: PartialEq<VL>,
{
	match (a, b) {
		(rdf_types::Term::Id(Id::Blank(_)), rdf_types::Term::Id(Id::Blank(_))) => true,
		(rdf_types::Term::Id(Id::Iri(a)), rdf_types::Term::Id(Id::Iri(b))) => a == b,
		(rdf_types::Term::Literal(a), rdf_types::Term::Literal(b)) => a == b,
		_ => false,
	}
}

#[cfg(test)]
mod tests {
	use crate::HashDataset;
	use rdf_types::{BlankId, BlankIdBuf, Id, Quad, Term};
	use static_iref::iri;

	#[test]
	fn substitution_01() {
		let mut a: HashDataset = HashDataset::new();
		let mut b = HashDataset::new();

		a.insert(Quad(
			Term::Id(Id::Blank(BlankIdBuf::from_u8(0))),
			Term::Id(Id::Iri(iri!("http://example.com/0").to_owned())),
			Term::Id(Id::Iri(iri!("http://example.com/1").to_owned())),
			None,
		));

		b.insert(Quad(
			Term::Id(Id::Blank(BlankIdBuf::from_u8(1))),
			Term::Id(Id::Iri(iri!("http://example.com/0").to_owned())),
			Term::Id(Id::Iri(iri!("http://example.com/1").to_owned())),
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
			Term::Id(Id::Blank(BlankIdBuf::from_u8(0))),
			Term::Id(Id::Iri(iri!("http://example.com/0").to_owned())),
			Term::Id(Id::Iri(iri!("http://example.com/1").to_owned())),
			None,
		));

		b.insert(Quad(
			Term::Id(Id::Blank(BlankIdBuf::from_u8(1))),
			Term::Id(Id::Iri(iri!("http://example.com/0").to_owned())),
			Term::Id(Id::Iri(iri!("http://example.com/2").to_owned())),
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
			Term::Id(Id::Blank(BlankIdBuf::from_u8(0))),
			Term::Id(Id::Iri(iri!("http://example.com/0").to_owned())),
			Term::Id(Id::Iri(iri!("http://example.com/1").to_owned())),
			None,
		));

		b.insert(Quad(
			Term::Id(Id::Blank(BlankIdBuf::from_u8(1))),
			Term::Id(Id::Iri(iri!("http://example.com/2").to_owned())),
			Term::Id(Id::Iri(iri!("http://example.com/1").to_owned())),
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
			Term::Id(Id::Blank(BlankIdBuf::from_u8(0))),
			Term::Id(Id::Iri(iri!("http://example.com/0").to_owned())),
			Term::Id(Id::Iri(iri!("http://example.com/1").to_owned())),
			None,
		));

		b.insert(Quad(
			Term::Id(Id::Blank(BlankIdBuf::from_u8(1))),
			Term::Id(Id::Iri(iri!("http://example.com/0").to_owned())),
			Term::Id(Id::Iri(iri!("http://example.com/1").to_owned())),
			Some(Term::Id(Id::Iri(iri!("http://example.com/2").to_owned()))),
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
			Term::Id(Id::Blank(BlankIdBuf::from_u8(0))),
			Term::Id(Id::Iri(iri!("http://example.com/0").to_owned())),
			Term::Id(Id::Iri(iri!("http://example.com/1").to_owned())),
			None,
		));
		a.insert(Quad(
			Term::Id(Id::Blank(BlankIdBuf::from_u8(1))),
			Term::Id(Id::Blank(BlankIdBuf::from_u8(0))),
			Term::Id(Id::Iri(iri!("http://example.com/1").to_owned())),
			None,
		));
		a.insert(Quad(
			Term::Id(Id::Blank(BlankIdBuf::from_u8(2))),
			Term::Id(Id::Blank(BlankIdBuf::from_u8(0))),
			Term::Id(Id::Iri(iri!("http://example.com/2").to_owned())),
			None,
		));

		b.insert(Quad(
			Term::Id(Id::Blank(BlankIdBuf::from_u8(10))),
			Term::Id(Id::Iri(iri!("http://example.com/0").to_owned())),
			Term::Id(Id::Iri(iri!("http://example.com/1").to_owned())),
			None,
		));
		b.insert(Quad(
			Term::Id(Id::Blank(BlankIdBuf::from_u8(11))),
			Term::Id(Id::Blank(BlankIdBuf::from_u8(10))),
			Term::Id(Id::Iri(iri!("http://example.com/1").to_owned())),
			None,
		));
		b.insert(Quad(
			Term::Id(Id::Blank(BlankIdBuf::from_u8(12))),
			Term::Id(Id::Blank(BlankIdBuf::from_u8(10))),
			Term::Id(Id::Iri(iri!("http://example.com/2").to_owned())),
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
