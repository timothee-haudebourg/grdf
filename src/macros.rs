#[macro_export]
macro_rules! reflect_graph_impl {
	() => {
		/// Checks if the graph is empty.
		pub fn is_empty(&self) -> bool {
			$crate::Graph::is_empty(self)
		}

		/// Returns any triple matching the given pattern.
		pub fn any_match(
			&self,
			pattern: Triple<
				Option<&<Self as $crate::Graph>::Subject>,
				Option<&<Self as $crate::Graph>::Predicate>,
				Option<&<Self as $crate::Graph>::Object>,
			>,
		) -> Option<
			Triple<
				&<Self as $crate::Graph>::Subject,
				&<Self as $crate::Graph>::Predicate,
				&<Self as $crate::Graph>::Object,
			>,
		> {
			$crate::Graph::any_match(self, pattern)
		}
	};
}

#[macro_export]
macro_rules! reflect_dataset_impl {
	() => {
		/// Get the default graph of the dataset.
		///
		/// This is the same as `graph(None)`.
		#[inline(always)]
		pub fn default_graph(&self) -> &<Self as $crate::Dataset>::Graph {
			$crate::Dataset::default_graph(self)
		}

		/// Returns the number of quads in the dataset.
		#[inline(always)]
		pub fn len(&self) -> usize {
			$crate::Dataset::len(self)
		}

		/// Returns the number of quads in the dataset.
		#[inline(always)]
		pub fn is_empty(&self) -> bool {
			$crate::Dataset::is_empty(self)
		}

		/// Iterate through all the subjects of the given graph.
		#[inline(always)]
		pub fn subjects<'a>(
			&'a self,
			id: Option<&<Self as $crate::Dataset>::GraphLabel>,
		) -> Option<<<Self as $crate::Dataset>::Graph as $crate::Graph>::Subjects<'a>>
		where
			<Self as $crate::Dataset>::Graph: 'a,
			<Self as $crate::Dataset>::Subject: 'a,
			<Self as $crate::Dataset>::Predicate: 'a,
			<Self as $crate::Dataset>::Object: 'a,
		{
			$crate::Dataset::subjects(self, id)
		}

		/// Iterate through all the predicates of the given subject of the given
		/// graph.
		#[inline(always)]
		pub fn predicates<'a>(
			&'a self,
			id: Option<&<Self as $crate::Dataset>::GraphLabel>,
			subject: &<Self as $crate::Dataset>::Subject,
		) -> Option<<<Self as $crate::Dataset>::Graph as $crate::Graph>::Predicates<'a>>
		where
			<Self as $crate::Dataset>::Graph: 'a,
			<Self as $crate::Dataset>::Predicate: 'a,
			<Self as $crate::Dataset>::Object: 'a,
		{
			$crate::Dataset::predicates(self, id, subject)
		}

		/// Iterate through all the objects of the given subject and predicate of the
		/// given graph.
		#[inline(always)]
		pub fn objects<'a>(
			&'a self,
			id: Option<&<Self as $crate::Dataset>::GraphLabel>,
			subject: &<Self as $crate::Dataset>::Subject,
			predicate: &<Self as $crate::Dataset>::Predicate,
		) -> Option<<<Self as $crate::Dataset>::Graph as $crate::Graph>::Objects<'a>>
		where
			<Self as $crate::Dataset>::Graph: 'a,
			<Self as $crate::Dataset>::Object: 'a,
		{
			$crate::Dataset::objects(self, id, subject, predicate)
		}
	};
}

#[macro_export]
macro_rules! reflect_sized_dataset_impl {
	() => {
		/// Consumes the dataset and returns the default graph.
		#[inline(always)]
		pub fn into_default_graph(self) -> <Self as $crate::Dataset>::Graph {
			$crate::SizedDataset::into_default_graph(self)
		}

		/// Consumes the dataset and returns an iterator over the subjects of the
		/// given graph.
		#[inline(always)]
		pub fn into_subjects(
			self,
			id: Option<&<Self as $crate::Dataset>::GraphLabel>,
		) -> Option<<<Self as $crate::Dataset>::Graph as $crate::SizedGraph>::IntoSubjects> {
			$crate::SizedDataset::into_subjects(self, id)
		}

		/// Consumes the dataset and returns an iterator over the predicates of the
		/// given subject of the given graph.
		#[inline(always)]
		pub fn into_predicates(
			self,
			id: Option<&<Self as $crate::Dataset>::GraphLabel>,
			subject: &<Self as $crate::Dataset>::Subject,
		) -> Option<<<Self as $crate::Dataset>::Graph as $crate::SizedGraph>::IntoPredicates> {
			$crate::SizedDataset::into_predicates(self, id, subject)
		}

		/// Consumes the dataset and returns an iterator over the objects of the
		/// given subject and predicate of the given graph.
		#[inline(always)]
		pub fn into_objects(
			self,
			id: Option<&<Self as $crate::Dataset>::GraphLabel>,
			subject: &<Self as $crate::Dataset>::Subject,
			predicate: &<Self as $crate::Dataset>::Predicate,
		) -> Option<<<Self as $crate::Dataset>::Graph as $crate::SizedGraph>::IntoObjects> {
			$crate::SizedDataset::into_objects(self, id, subject, predicate)
		}
	};
}

#[macro_export]
macro_rules! reflect_mutable_dataset_impl {
	() => {
		/// Get the default graph mutably.
		///
		/// Note to implementors: the default graph should always exists.
		#[inline(always)]
		pub fn default_graph_mut(&mut self) -> &mut <Self as $crate::Dataset>::Graph {
			$crate::MutableDataset::default_graph_mut(self)
		}
	};
}

pub use reflect_dataset_impl;
pub use reflect_graph_impl;
pub use reflect_mutable_dataset_impl;
pub use reflect_sized_dataset_impl;
