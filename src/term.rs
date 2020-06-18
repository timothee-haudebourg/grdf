//! Built-in gRDF node type.

use iref::IriBuf;
use ordered_float::NotNan;
use std::fmt;

/// Standard gRDF term.
///
/// Either a blank node identifier, IRI or literal value.
#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub enum Term {
	/// Blank node identifier.
	Blank(usize),

	/// IRI.
	Iri(IriBuf),

	/// Literal value.
	Value(Literal),
}

impl Term {
	/// Get the string representation of the term, if any.
	///
	/// Returns some string if the term is an IRI or a string literal.
	pub fn as_str(&self) -> Option<&str> {
		match self {
			Term::Iri(iri) => Some(iri.as_str()),
			Term::Value(lit) => lit.as_str(),
			_ => None,
		}
	}
}

impl fmt::Display for Term {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Term::Blank(id) => write!(f, "_:{}", id),
			Term::Iri(iri) => write!(f, "<{}>", iri),
			Term::Value(lit) => lit.fmt(f),
		}
	}
}

/// RDF literals.
#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub enum Literal {
	/// A maybe tagged string literal.
	String(String, Option<Tag>),

	/// Integer constant.
	Int(i64),

	/// Fload constant.
	Float(NotNan<f64>),

	/// Boolean constant.
	Bool(bool),
}

impl fmt::Display for Literal {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Literal::String(str, Some(tag)) => write!(f, "\"{}\"{}", str, tag),
			Literal::String(str, None) => write!(f, "\"{}\"", str),
			Literal::Int(i) => i.fmt(f),
			Literal::Float(d) => d.fmt(f),
			Literal::Bool(true) => write!(f, "true"),
			Literal::Bool(false) => write!(f, "false"),
		}
	}
}

impl Literal {
	pub fn as_str(&self) -> Option<&str> {
		match self {
			Literal::String(str, _) => Some(str.as_str()),
			_ => None,
		}
	}
}

/// Tag of a RDF tagged string.
#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub enum Tag {
	/// Language tag.
	Lang(String),

	/// IRI tag.
	Iri(IriBuf),
}

impl fmt::Display for Tag {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Tag::Lang(lang) => write!(f, "@{}", lang),
			Tag::Iri(iri) => write!(f, "^^<{}>", iri),
		}
	}
}
