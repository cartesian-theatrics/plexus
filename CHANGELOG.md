# Changelog

## Unreleased

- Validate concave navigation and sweeps on bowls, torus interiors, V-valleys,
  and narrow trenches. Smooth frame normals within physical face fans, coalesce
  stations that would fold locally, and reject unsafe sweeps with source-aware
  `:surface-sweep-fold` errors.

- Add portable surface navigation: prepared surfaces, attachment/detachment,
  geodesic forward and sideways walks, normal depth, heading turns, and bent sweeps.
- Preserve surface cursors through branches and rebuilds, with SCI source macros
  and static option checks. See [surface navigation](docs/surface-navigation.md)
  for geometric limits and supported operations.

## 2.0.0 — 2026-09-24

Plexus now supports Clojure, ClojureScript/WASM, and optional SCI interpretation
with a source-aware macro DSL.

- Move the modeling runtime, schemas, transforms, and helpers to `.cljc`.
- Expose validated operation functions with keyword or map options, nested
  sequences, higher-order composition, and CSG result expressions.
- Add `plexus.source` macros for compiled Clojure/ClojureScript and SCI hosts.
  Errors retain the offending expression, filename, line, and column through
  nested operations, result expressions, and deferred rebuilding.
- Add `plexus.sci/config` and exported clj-kondo checks. SCI remains optional.
- Preserve forward sampling defaults, including explicit nil step counts, and
  document the comparison with existing `scad-etc` models.
- Test JavaScript against the published geometry dependency and its bundled
  WASM assets; no sibling checkout is required.

### Migration

Operation arguments are now evaluated once when the form is constructed.
To recompute values during `rebuild`, wrap forms in `p/deferred`:

```clojure
(p/deferred #(p/forward :length @length))
```

The internal callable `Form` type and `defop` macro are removed. Use operation
functions and form data instead. For automatic source locations, use the
optional `plexus.source` macros. `defmodel` and `export-models` remain JVM-only.

### Dependencies

All direct Maven dependencies and development tools in `deps.edn` use the latest
stable versions available when this release was prepared:

| Dependency | Version |
| --- | --- |
| Clojure | 1.12.6 |
| clj-manifold3d | 1.2.0 |
| Malli | 0.20.1 |
| Native Manifold (JVM development alias) | 2.2.0 |
| SCI (optional) | 0.15.58 |
| Shadow CLJS | 3.5.3 |
| Kaocha | 1.91.1392 |
| Badigeon | 1.7 (unchanged) |

JVM consumers still add the native Manifold artifact for their platform
separately. The `:local-dev` alias retains sibling checkout overrides.

### Validation

- JVM: 39 tests, 111 assertions, no failures.
- JavaScript/WASM: 31 tests, 92 assertions, no failures or errors.
- Advanced JavaScript compilation: no warnings.
- Static option checks: valid fixture accepted; eight invalid options located.
