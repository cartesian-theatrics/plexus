# Concavity validation

The regression suite in `test/main/plexus/concavity_test.cljc` runs on both JVM
and JavaScript/WASM, using the same published geometry dependencies.

Verified: 56 JVM tests / 295 assertions and 48 JavaScript tests / 276 assertions,
with no failures or errors. Advanced JavaScript compilation reports no warnings.

## Cases and results

| Reference surface | Checks | Result |
| --- | --- | --- |
| V-valley with a 90-degree inward crease | Analytic endpoint and heading, diagonal sideways/forward travel, arc length, reversal, 1/17/113 sampling steps | Navigation passes |
| 0.2-unit-wide connected trench | Descend one wall, traverse the floor, ascend the facing wall; reverse the route | Follows topology without jumping across the gap |
| Spherical Boolean-cut bowl | Walk from the bottom up the wall; positive, zero, and negative depth; spacing 1 and 0.1 | Sweeps pass clearance and transverse-intersection checks |
| Torus starting at its concave inner wall | Walk through changing curvature; positive, zero, and negative depth; spacing 1 and 0.1 | Sweeps pass clearance and transverse-intersection checks |
| V-valley swept at depth 0 or +0.5 | A 0.2 × 0.2 profile across the sharp inward crease | Rejected with `:surface-sweep-fold` |
| V-valley swept at depth −0.5 | Same profile on the outside of this turn | Passes the tested intersection and containment checks |
| Bowl with depth 11, exceeding its radius | Offset path folds past the curvature center | Rejected with `:surface-sweep-fold` |

For the bowl and torus, the profile is 0.2 × 0.2 units, depth is −0.5, 0, or +0.5,
and lengths are 8 and 12 respectively. At positive depth the generated solid has
less than 1e−6 cubic units of intersection with its reference solid. At negative
depth it has less than 1e−6 cubic units outside it. At zero depth it straddles the
reference surface. Inside plus outside volume agrees with total volume within
1e−5 cubic units. These clearances are specific to these fixtures and dimensions.

Navigation checks use known endpoints where available, measured polyline length,
reverse paths, triangle barycentric coordinates, and unit tangent headings.
The bowl also checks continuity of interpolated normals across physical edges
and alignment with its independently known inward radial direction.

## Failure found and corrected

The original sweep used discontinuous face normals. At sharp inward corners its
centerline was correct, but the loft could self-intersect. Native Manifold status
still returned `:NoError`. Increasing the number of samples did not resolve it.
Closely spaced stations could also invert small parts of a profile near edges of
otherwise smooth meshes.

The regression suite deliberately reconstructs an unchecked bad valley loft. It
asserts that native status succeeds while a separate triangle-intersection
checker detects transverse intersections. The guarded DSL rejects that sweep,
including when called through SCI source macros; operation and source-expression
metadata survive the error.

The correction uses angle-weighted frame normals within smooth physical face
fans, preserving creases over 60 degrees. Before lofting, it checks progression
of every profile vertex relative to both neighboring sweep directions. In smooth
regions, nearby stations that would fold can be coalesced, bounded by twice the
requested spacing. Sharp folds and unrecoverable collapses are rejected rather
than silently returned as geometry. Navigation samples and endpoints are retained.

## Scope of the geometric validation

The independent checker tests segment/triangle intersections on nonadjacent
output triangles, with bounding-box rejection. It detects transverse crossings;
it is not an exhaustive proof against all coplanar overlaps, contacts at shared
vertices, or arbitrarily small numerical defects. Native topology/status checks
and Boolean volume/clearance checks complement it.

The runtime guard checks local sweep inversion. It does not detect collisions
between distant parts of a path or with unrelated geometry, and it does not
create mitered or rounded joins at sharp creases. A surrounding `loft` that
changes profiles needs its own geometric review. Profiles still remain rigid
across their width. The passing fixtures do not imply every concave surface,
profile size, or depth is safe.

## Reproduce

```sh
clojure -M:dev:test:sci
clojure -M:cljs scripts/prepare-cljs.clj
clojure -M:cljs -m shadow.cljs.devtools.cli release test
node target/cljs-tests.js
clojure -M:dev src/examples/surface_concavity.clj
```

The example writes `target/surface-concave-bowl.glb` and
`target/surface-concave-torus.glb`. Orange geometry is the generated sweep; gray
geometry is the reference surface.
