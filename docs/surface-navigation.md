# Surface navigation

A surface frame treats local **Z as forward**, **X as sideways**, and **Y as depth
along the outward normal**. `forward` sweeps the current cross-section along the
walked path. The API works in Clojure, ClojureScript/WASM, and the SCI adapter.

```clojure
(require '[clj-manifold3d.core :as m]
         '[plexus.core :as p])

(def support (m/cylinder 30 20 20 96))
(def surface (p/surface support))

(def ribbon
  (p/extrude
    (p/frame :name :ribbon :cross-section (m/square 1.2 0.6 true))
    (p/on-surface :surface surface
                  :origin [20 0 8.37] :direction [0 1 0.15]
                  :depth 0.45 :step-size 0.5)
    (p/forward :length 65)
    (p/rotate :y 0.7)
    (p/forward :length 12)
    (p/translate :y 1)
    (p/forward :length 8)
    (p/save-transform :frame :ribbon :name :end)))
```

The original support is a navigation reference; it is not automatically included
in the extrusion. Union the ribbon with it for a raised feature, or place a
cutter at negative depth and subtract it for a groove.

For a colored GLB with branching paths, run:

```sh
clojure -M:dev src/examples/surface_navigation.clj
# writes target/surface-navigation.glb
```

## Attaching and moving

`p/surface` copies a Manifold's triangle topology into immutable Clojure data.
Prepare it once and share it between paths. It retains no native handles; the
original Manifold may be disposed after preparation. `on-surface` also accepts
a raw Manifold, preparing it during each build.

`on-surface` requires `:surface`, `:origin`, and `:direction`. Both vectors use
world coordinates. The origin is snapped once to the nearest triangle, and the
direction is projected into its tangent plane. At a seed edge or vertex, the
direction chooses an outgoing face. Start near the intended sheet when surfaces
are close together. An empty surface, zero direction, or direction with no
outgoing tangent is an error.

| Operation | Surface behavior |
| --- | --- |
| `forward :length d` / `forward :z d` | Walk forward and sweep a cross-section |
| `forward :x d` | Walk sideways and sweep a cross-section normal to that movement |
| `forward :y d` | Extrude along the normal, changing depth at the same surface point |
| `translate :x x :z z :y y` | Walk one combined tangent displacement, then change depth; create no geometry |
| `rotate :y angle` | Turn the forward direction about the outward normal, in radians |
| `off-surface` | Keep the current position and orientation, resume ordinary spatial operations |

All distances are in model units and may be negative. Tangential distance is
measured on the reference surface, regardless of depth. Depth is an absolute
offset from that surface, not a repeated nearest-point projection.

`:to` targets specific frames. Without it, attachment and detachment affect the
active frames and the default frame. New frames inherit the default frame's
surface cursor. Branches restore existing frames' cursors afterward, while
retaining generated geometry and saved transforms. `save-transform` returns an
ordinary world-space transform suitable for placing another model.

`forward` supports `:gap`, `:center`, `:branch?`, and `:n-steps`. `:step-size`
(default 1) supplies the sampling interval when `:n-steps` is absent or nil.
Triangle crossings are always included, even with `:n-steps 1`. Existing `loft`
can interpolate between changing cross-sections along these samples. Surface
path samples returned by `points` use world coordinates, including when the
frame was created under a nonidentity transform.

Use `p/deferred` when a rebuilt path should read new parameter values, as with
ordinary Plexus operations. Use `plexus.source/on-surface`, `forward`, etc. for
file/line/column diagnostics. These macros are also available inside SCI.

## Geometry and limits

Navigation follows the **triangulated surface**, not an inferred analytic sphere
or spline. The heading is parallel transported by unfolding adjacent triangles.
Walking straight defines a local geodesic initial-value path; it is not a solver
for the globally shortest route to a destination. Unlike a planar chart, it can
continue around a cylinder and past the seed tangent plane's horizon.

The implementation follows the topology discipline in Manifold's native
`SurfaceUV` mapping: property vertices are connected through explicit merge IDs
and paired physical edges. It never welds nearby coordinates or repeatedly
snaps to the closest triangle. Thus a walk cannot jump between nearby,
disconnected sheets. Native UV mapping uses plane cuts; Plexus instead transports
the heading across edges. See also Geometry Central's description of
[geodesic path tracing](https://geometry-central.net/surface/algorithms/geodesic_paths/).

Each sampled cross-section is rigid and oriented to the face normal. The sweep
bends along its centerline; it does **not** independently map every point across
the profile's width onto the surface. Use a sufficiently fine reference mesh and
a narrow profile for a surface-following ribbon. Wide profiles, sharp creases,
large depth offsets, and paths crossing themselves can produce overlaps or
self-intersections. Depth and orientation are discontinuous at sharp creases;
this version does not build mitered or rounded joins there.

When a walk reaches a vertex with nonzero angle defect, continuation is ambiguous
and raises `:reason :vertex-singularity`, with the point and face in `ex-data`.
Change the origin or heading. Starting at a vertex is allowed because the supplied
direction selects the initial face. Flat subdivision vertices can be crossed.
Ending exactly on an edge retains the arrival face; its normal may differ from
the normal of a path arriving from the other direction.

Refine genuinely smooth tangent meshes before preparing them; zero-filled tangent
buffers from Boolean composition are accepted. Preparation and seed selection
scan the mesh; ordinary movement visits only crossed triangles, except for
vertex-fan checks. Individual movements are limited to 100,000 crossings/samples.

Surface mode rejects spatial `left/right/up/down/curve`, X/Z tilts, global
translation, replacement transforms, `insert :end-frame`, and `forward` options
`:model`, `:twist`, and `:transform-step-fn`. Turn with `rotate :y` and walk with
`forward`, or call `off-surface` before those spatial operations. Regular `insert`
can place a rigid model at the surface frame without changing the cursor.
