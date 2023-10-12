use crate::{Dataset, GraphAccess, IdentityAccess};

pub trait DatasetAccess<D: ?Sized + Dataset>: GraphAccess<D::Graph> {
	fn subject_as_graph<'a>(
		&self,
		dataset: &'a D,
		subject: &'a D::Subject,
	) -> Option<&'a D::GraphLabel>;
}

impl<D: ?Sized + Dataset> DatasetAccess<D> for () {
	fn subject_as_graph<'a>(
		&self,
		_dataset: &'a D,
		_subject: &'a <D as Dataset>::Subject,
	) -> Option<&'a <D as Dataset>::GraphLabel> {
		None
	}
}

impl<
		D: ?Sized + Dataset<Subject = <D as Dataset>::Object, GraphLabel = <D as Dataset>::Subject>,
	> DatasetAccess<D> for IdentityAccess
{
	fn subject_as_graph<'a>(
		&self,
		_dataset: &'a D,
		subject: &'a <D as Dataset>::Subject,
	) -> Option<&'a <D as Dataset>::GraphLabel> {
		Some(subject)
	}
}

pub struct View<'a, D: ?Sized + Dataset, A> {
	pub dataset: &'a D,
	pub graph_label: Option<&'a D::GraphLabel>,
	pub graph: Option<&'a D::Graph>,
	pub subject: &'a D::Subject,
	pub access: A,
}

impl<'a, D: ?Sized + Dataset, A> View<'a, D, A> {
	pub fn new(
		dataset: &'a D,
		graph_label: Option<&'a D::GraphLabel>,
		graph: Option<&'a D::Graph>,
		subject: &'a D::Subject,
		access: A,
	) -> Self {
		Self {
			dataset,
			graph_label,
			graph,
			subject,
			access,
		}
	}
}
