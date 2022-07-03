//! Compatibility layer with the [`locspan`](https://crates.io/crates/locspan) library.
//!
//! Provides dataset implementations that also saves the metadata of each quad.
use locspan::Meta;
use rdf_types::meta::MetaQuad;
use std::hash::Hash;

/// [`Quad`](rdf_types::Quad) with metadata where each component if borrowed.
pub type MetaQuadRef<'s, 'p, 'o, 'g, 'm, S, P, O, G, M> = Meta<
	rdf_types::Quad<
		Meta<&'s S, &'m M>,
		Meta<&'p P, &'m M>,
		Meta<&'o O, &'m M>,
		Meta<&'g G, &'m M>,
	>,
	&'m M,
>;

/// Quad of metadata.
pub type QuadMetadata<M> =
	rdf_types::Quad<M, M, M, M>;

/// Quad object with location information for the entire quad.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Object<O, M> {
	/// Actual object value.
	pub value: O,

	/// Metadata of quad.
	///
	/// The outer [`Meta`] stores the metadata of the quad,
	/// while the inner [`QuadMetadata`] structure stores the
	/// metadata of each quad component.
	pub metadata: Meta<QuadMetadata<M>, M>,
}

impl<O, M> Object<O, M> {
	/// Creates a new quad where all location information is stored in the `Object<O, F>`.
	pub fn new<S, P, G>(
		Meta(rdf_types::Quad(Meta(s, s_meta), Meta(p, p_meta), Meta(o, o_meta), g), meta): MetaQuad<
			S,
			P,
			O,
			G,
			M,
		>,
	) -> rdf_types::Quad<S, P, Self, G> {
		let (metadata, g) = match g {
			Some(Meta(g, g_meta)) => (QuadMetadata::new(s_meta, p_meta, o_meta, Some(g_meta)), Some(g)),
			None => (QuadMetadata::new(s_meta, p_meta, o_meta, None), None),
		};

		rdf_types::Quad(
			s,
			p,
			Object {
				value: o,
				metadata: Meta(metadata, meta),
			},
			g,
		)
	}

	pub fn as_meta_quad<'s, 'p, 'g, S, P, G>(
		&self,
		s: &'s S,
		p: &'p P,
		g: Option<&'g G>,
	) -> MetaQuadRef<'s, 'p, '_, 'g, '_, S, P, O, G, M> {
		Meta(
			rdf_types::Quad(
				Meta(s, self.metadata.subject()),
				Meta(p, self.metadata.predicate()),
				Meta(&self.value, self.metadata.object()),
				g.map(|g| Meta(g, self.metadata.graph().unwrap())),
			),
			self.metadata.metadata(),
		)
	}

	pub fn into_meta_quad<S, P, G>(self, s: S, p: P, g: Option<G>) -> MetaQuad<S, P, O, G, M> {
		let Meta(metadata, meta) = self.metadata;
		let g_meta = metadata.3;

		Meta(
			rdf_types::Quad(
				Meta(s, metadata.0),
				Meta(p, metadata.1),
				Meta(self.value, metadata.2),
				g.map(|g| Meta(g, g_meta.unwrap())),
			),
			meta,
		)
	}
}

/// `HashDataset` with location information.
pub type HashDataset<S, P, O, G, M> = crate::HashDataset<S, P, Object<O, M>, G>;

impl<S: Eq + Hash, P: Eq + Hash, O: Eq + Hash, G: Eq + Hash, M: Eq + Hash>
	HashDataset<S, P, O, G, M>
{
	/// Inserts a located quad into the dataset.
	pub fn loc_insert(&mut self, quad: MetaQuad<S, P, O, G, M>) {
		self.insert(Object::new(quad))
	}

	/// Creates a borrowing iterator over the located quads of the dataset.
	pub fn loc_quads(&self) -> MetaQuads<Self> {
		MetaQuads(self.quads())
	}

	/// Turns this dataset into an iterator over the located quads of the dataset.
	pub fn into_meta_quads(self) -> IntoMetaQuads<Self>
	where
		S: Clone,
		P: Clone,
		G: Clone,
	{
		IntoMetaQuads(self.into_quads())
	}
}

impl<S: Eq + Hash, P: Eq + Hash, O: Eq + Hash, G: Eq + Hash, M: Eq + Hash>
	std::iter::Extend<MetaQuad<S, P, O, G, M>> for HashDataset<S, P, O, G, M>
{
	fn extend<I: IntoIterator<Item = MetaQuad<S, P, O, G, M>>>(&mut self, iter: I) {
		for quad in iter {
			self.loc_insert(quad)
		}
	}
}

impl<S: Eq + Hash, P: Eq + Hash, O: Eq + Hash, G: Eq + Hash, M: Eq + Hash>
	std::iter::FromIterator<MetaQuad<S, P, O, G, M>> for HashDataset<S, P, O, G, M>
{
	fn from_iter<I: IntoIterator<Item = MetaQuad<S, P, O, G, M>>>(iter: I) -> Self {
		let mut dataset = Self::new();
		dataset.extend(iter);
		dataset
	}
}

/// `BTreeDataset` with location information.
pub type BTreeDataset<S, P, O, G, M> = crate::BTreeDataset<S, P, Object<O, M>, G>;

impl<S: Ord, P: Ord, O: Ord, G: Ord, M: Ord> BTreeDataset<S, P, O, G, M> {
	/// Inserts a located quad into the dataset.
	pub fn loc_insert(&mut self, quad: MetaQuad<S, P, O, G, M>) {
		self.insert(Object::new(quad))
	}

	/// Creates a borrowing iterator over the located quads of the dataset.
	pub fn loc_quads(&self) -> MetaQuads<Self> {
		MetaQuads(self.quads())
	}

	/// Turns this dataset into an iterator over the located quads of the dataset.
	pub fn into_meta_quads(self) -> IntoMetaQuads<Self>
	where
		S: Clone,
		P: Clone,
		G: Clone,
	{
		IntoMetaQuads(self.into_quads())
	}
}

impl<S: Ord, P: Ord, O: Ord, G: Ord, M: Ord> std::iter::Extend<MetaQuad<S, P, O, G, M>>
	for BTreeDataset<S, P, O, G, M>
{
	fn extend<I: IntoIterator<Item = MetaQuad<S, P, O, G, M>>>(&mut self, iter: I) {
		for quad in iter {
			self.loc_insert(quad)
		}
	}
}

impl<S: Ord, P: Ord, O: Ord, G: Ord, M: Ord>
	std::iter::FromIterator<MetaQuad<S, P, O, G, M>> for BTreeDataset<S, P, O, G, M>
{
	fn from_iter<I: IntoIterator<Item = MetaQuad<S, P, O, G, M>>>(iter: I) -> Self {
		let mut dataset = Self::new();
		dataset.extend(iter);
		dataset
	}
}

pub struct MetaQuads<'a, D: 'a + crate::Dataset>(D::Quads<'a>);

impl<'a, O: 'a, M: 'a, D: 'a + crate::Dataset<Object = Object<O, M>>> Iterator
	for MetaQuads<'a, D>
{
	type Item = Meta<
		rdf_types::Quad<
			Meta<&'a D::Subject, &'a M>,
			Meta<&'a D::Predicate, &'a M>,
			Meta<&'a O, &'a M>,
			Meta<&'a D::GraphLabel, &'a M>,
		>,
		&'a M,
	>;

	fn next(&mut self) -> Option<Self::Item> {
		self.0
			.next()
			.map(|rdf_types::Quad(s, p, o, g)| o.as_meta_quad(s, p, g))
	}
}

pub struct IntoMetaQuads<D: crate::SizedDataset>(D::IntoQuads)
where
	D::Graph: crate::SizedGraph;

impl<O, M, D: crate::SizedDataset<Object = Object<O, M>>> Iterator for IntoMetaQuads<D>
where
	D::Graph: crate::SizedGraph,
{
	type Item = MetaQuad<D::Subject, D::Predicate, O, D::GraphLabel, M>;

	fn next(&mut self) -> Option<Self::Item> {
		self.0
			.next()
			.map(|rdf_types::Quad(s, p, o, g)| o.into_meta_quad(s, p, g))
	}
}
