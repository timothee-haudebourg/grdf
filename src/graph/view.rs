use crate::Graph;

/// Specifies how to access a graph and reinterpret triples objects as subjects.
pub trait GraphAccess<G: ?Sized + Graph> {
	/// Returns the given graph triple object as a subject, if possible.
	fn object_as_subject<'a>(&self, graph: &'a G, object: &'a G::Object) -> Option<&'a G::Subject>;
}

impl<G: ?Sized + Graph> GraphAccess<G> for () {
	fn object_as_subject<'a>(
		&self,
		_graph: &'a G,
		_object: &'a <G as Graph>::Object,
	) -> Option<&'a <G as Graph>::Subject> {
		None
	}
}

/// Simple `GraphAccess` implementation for graphs such that `Subject = Object`.
pub struct IdentityAccess;

impl<G: ?Sized + Graph<Subject = <G as Graph>::Object>> GraphAccess<G> for IdentityAccess {
	fn object_as_subject<'a>(
		&self,
		_graph: &'a G,
		object: &'a <G as Graph>::Object,
	) -> Option<&'a <G as Graph>::Subject> {
		Some(object)
	}
}

/// View a graph from the perspective of a single subject resource.
pub struct GraphView<'a, G: ?Sized + Graph, A> {
	pub graph: &'a G,
	pub subject: &'a G::Subject,
	pub access: A,
}

impl<'a, G: ?Sized + Graph, A> GraphView<'a, G, A> {
	pub fn new(graph: &'a G, subject: &'a G::Subject, access: A) -> Self {
		Self {
			graph,
			subject,
			access,
		}
	}
}
