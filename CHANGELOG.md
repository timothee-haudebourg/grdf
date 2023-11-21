# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.22.1] - 2023-11-21

### Added

- [188175e] Add `serde` feature.

## [0.22.0] - 2023-10-20

### Build

- [1b91c39] Upgrade `rdf-types` to version 0.18
- [1b91c39] Upgrade `locspan` to version 0.8
- [5f94c3f] Drop `derivative`, use `educe`.

## [0.21.0] - 2023-10-18

### Added

- [3ecaf3c] Add `GraphView`.
- [4d6fa71] Add `IdentityAccess` type.

### Build

- [03078fd] Upgrade `rdf-types` to version 0.17.0

### Removed

- [6ccd5da] Remove local path for `linked-data`.
- [231aca4] Remove `linked-data` dependency.
- [00c3e5a] Remove `im` dependency.

## [0.19.0] - 2023-06-06

### Build

- [9bdca0c] Upgrade `rdf-types` to version 0.15

## [0.18.0] - 2023-02-28

### Build

- [ecc88ae] Upgrade `rdf-types` from `0.13` to `0.14.2`.

## [0.17.0] - 2023-02-27

### Build

- [cf14a5d] Upgrade `rdf_types` version from `0.12` to `0.13`.

### Removed

- [75807d5] Remove note about nightly compilers
- [1443af5] Remove `cliff` config from `Cargo.toml`.

## [0.16.3] - 2023-01-19

### Changed

- [6edc572] Change `&mut self` into `&self` for pattern matching methods.

## [0.16.2] - 2023-01-11

### Fixed

- [97dd6d9] Fix `take_match` functions.

## [0.16.1] - 2023-01-11

### Added

- [1320a5c] Add `BTreeGraph::remove` method (now public).
- [1320a5c] Add `BTreeGraph::take` method (now public).
- [1320a5c] Add `BTreeGraph::take_match` method (now public).
- [1320a5c] Add `HashGraph::remove` method (now public).
- [1320a5c] Add `HashGraph::take` method (now public).
- [1320a5c] Add `HashGraph::take_match` method (now public).

### Fixed

- [1320a5c] Fix methods visibility.

## [0.16.0] - 2023-01-11

### Changed

- [2e7b837] Change `std::collection::HashMap` to `hashbrown`.

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

