# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.15.3] - 2023-01-09

### Added

- [47969fb] Add `Clone` implementation.

## [0.15.2] - 2023-01-09

### Added

- [981adc6] Add `?Sized` bounds to query parameters.

## [0.15.1] - 2023-01-09

### Build

- [b5f5bd9] Upgrade rust version in CI.

### Changed

- [63532e1] Change `loc_insert` return type.

## [0.15.0] - 2023-01-09

### Added

- [457b59d] Add `len`/`is_empty` methods.
- [457b59d] Add `remove`method.
- [457b59d] Add `pattern_matching` method.
- [457b59d] Add `take`/`take_match` methods with `Take*` traits.
- [457b59d] Add `Graph`/`Dataset` methods in inherent implementations.

### Changed

- [457b59d] Change return type of `insert` methods.
- [457b59d] Change `SizedGraph::triples` into `::into_triples`.
- [457b59d] Change `SizedGraph::predicates` into `::into_predicates`.
- [457b59d] Change `SizedGraph::objects` into `::into_objects`.

## [0.14.1] - 2022-12-20

### Added

- [d7a7a31] Impl `Clone` for iterators.

## [0.14.0] - 2022-10-20

### Build

- [515c0df] Upgrade dependencies.

## [0.13.0] - 2022-10-20

### Added

- [6e7f46d] Add some `where` bounds on the associated `Iter` type.
- [e4056e4] Add newly required lifetime bounds.
- [085237b] Add FromIterator impl & relax bounds.
- [e9c5ead] Add blank id substitution algorithm.
- [a6497c6] Add CI.
- [b685d77] Add `Ord` & `Hash` impl for `BTreeDataset`.
- [aa121f0] Add `FromIterator` and `Extend` impls.

### Build

- [f56e8cd] Upgrade `locpsan` to version 0.7
- [c149b42] Upgrade `rdf-types` to 0.8, move to 0.10
- [9781837] Upgrade `rdf-types` to 0.9
- [c80ad6b] Upgrade dependencies.
- [28ebabb] Upgrade dependencies.

### Changed

- [2062e24] Move to 0.2.0-beta

### Fixed

- [9d394f4] Fix grdf version.
- [87e0db6] Fix isomorphism interface.
- [56fda81] Fix non standard function name.
- [edeedc4] Fix CI clippy command.
- [1d89ebf] Fix nightly version in CI.
- [b075e1b] Fix clippy warnings.
- [35f533e] Fix ambiguous borrow.

### Removed

- [ba67439] Remove nasty hack.
- [f1066a0] Remove uses of `MutableDataset`
- [b94ee7b] Remove GAT feature gate.

## [0.1.1-beta] - 2020-06-18

### Added

- [e1e6571] impl `Display` for triples and quads.

