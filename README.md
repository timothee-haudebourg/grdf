# Generalized RDF

<table><tr>
  <td><a href="https://docs.rs/grdf">Documentation</a></td>
  <td><a href="https://crates.io/crates/grdf">Crate informations</a></td>
  <td><a href="https://github.com/timothee-haudebourg/grdf">Repository</a></td>
</tr></table>

The [Resource Description Framework (RDF)](https://en.wikipedia.org/wiki/Resource_Description_Framework)
is a powerful method for modeling data and knowledge
defined by the [World Wide Web Consorsium (W3C)](https://www.w3.org/).
A RDF dataset consists in a collection of graphs connecting nodes, values and predicates.
This crate provides traits and implementations of
[Generalized RDF (gRDF)](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#section-generalized-rdf)
where nodes, values and predicates have the same representation.

Note that this crates requires the nightly compiler.
It needs Generic Associated Typed (GAT) to work properly,
which are [still being implemented](https://github.com/rust-lang/rust/issues/44265).

 ## Basic usage

 ### Exploring a dataset

 Each `Dataset` implementation provides many iterators to explore the data.
 One simple way is to iterate through the quad of the dataset:

 ```rust
 for Quad(graph, subject, predicate, object) in dataset.quads() {
 	// do something
 }
 ```

 Another way is to access each graph individually using `Dataset::graph`.
 For a given graph, it is then possible to iterate through the triples of the
 graph:

 ```rust
 let graph = dataset.graph(id).unwrap();

 for Triple(subject, predicate, object) in graph.triples() {
 	// do something
 }
 ```

 It is also possible to explore the graph logically, subject by subject,
 predicate by predicate, object by object:

 ```rust
 // for each subject of the graph...
 for (subject, predicates) in graph.subjects() {
 	// for each predicate it is subject...
 	for (predicate, objects) in predicates {
 		// for each triple (subject, predicate, object)...
 		for object in objects {
 			// do something
 		}
 	}
 }
 ```

 ### Inserting new data

 Insertion can be done on `MutableDataset` implementations using
 `MutableDataset::insert`:

 ```rust
 let mut dataset = ...;
 dataset.insert(Quad(graph, subject, predicate, object));
 ```

 Again it is possible to access each graph of the dataset mutably:

 ```rust
 let mut graph = dataset.graph_mut(id).unwrap();
 graph.insert(Triple(subject, predicate, object));
 ```

 ### Custom node type

 The type used to represent RDF nodes (subjects, predicate and objects) is a
 parameter of the dataset. Anything can be used although this crate provide a
 default `Term` type that represents generic RDF nodes (blank nodes,
 IRI-named nodes and literal values).

## License

Licensed under either of

 * Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any
additional terms or conditions.
