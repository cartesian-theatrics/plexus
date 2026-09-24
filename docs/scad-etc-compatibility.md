# scad-etc compatibility audit

Compared the migration against the pre-migration Plexus sources, retaining the
existing forward-sampling and entering-cross-section loft fixes in both versions.
The scad-etc checkout was not modified.

## API surface

The source scan found 73 namespaces with analyzed Plexus imports. No references
to public vars removed by this migration were found. The only removed public var
in core/transforms/utils is `plexus.core/defop`; scad-etc does not use it.

22 namespaces have no unresolved Plexus API references. The remaining namespaces
include uses of older APIs such as `model`, `backward`, `spin`, `iso-hull`,
`plexus.segment`, and `plexus.math`, already absent before this migration. This is
a symbol audit, not a claim that every complete namespace loads or builds.

No uses of `rebuild`, `defop`, or `plexus.impl.Form` were found in scad-etc sources.
The change from implicit argument re-evaluation to explicit `deferred` therefore
does not require a rebuild call-site migration in this checkout.

## Regression found and fixed

`scad-etc.utils/calc-right-forward-left-segment` passes `:n-steps nil` when its
optional sampling count is omitted. The new positive-integer validation initially
rejected this, although the extrusion implementation uses nil to select its
default. The schema and static hook now accept nil while rejecting invalid
non-nil step counts.

This helper is used by `georid-house`, `lake-house`, and several mixer models.
Evaluating the actual helper definition in isolation produced identical paths on
both versions: 42 points with nil steps, 46 with five steps, ending at approximately
`[10 40]`. The shared JVM/JS tests also cover nil sampling.

```sh
clojure -Sdeps '{:paths ["src/main" "resources" "../scad-etc/src/main"]}' \
  -M:local-dev scripts/check-scad-helper.clj
```

## Model tests

| Check | Migrated result | Baseline comparison |
| --- | --- | --- |
| wave-truss suite | 5 tests, 902 assertions, all pass | Also passes |
| wave-truss + carport-frame suites | 8 tests, 923 assertions; 3 failures, 1 error | Same failures/error and assertion count |
| triangular-truss suite | 6 tests, 849 assertions; 2 connector-wall validation errors | Same errors and assertion count |
| actual shared curve helper | nil and five-step paths pass | Matching point counts and endpoints |

The carport failures are fixture/topology expectations in its tests. The
triangular-truss errors are its connector-wall checks. Neither was introduced by
the migration. The triangular comparison was also performed during the initial
migration validation.

A direct `require` of `scad-etc.utils` is blocked by an unregistered `#dbg` reader
in `scad_etc/offset_polygon.clj:109`, independently of Plexus. The helper check
reads and evaluates only the selected real function definition with its actual
math dependencies. Whole scratch namespaces with top-level exports were not
blindly loaded, so this audit does not certify every model in scad-etc.

## Source diagnostics

Macro-free constructors cannot automatically capture their call-site source
coordinates. `plexus.source` provides optional operation macros (and `at`) that
preserve file, line, column, and original expression while retaining single
argument evaluation. `plexus.core/with-source` accepts explicit locations without
macros, including in SCI. The SCI adapter also exposes `plexus.source` macros for
automatic locations on both JVM and JS hosts. Execution errors include the
offending form and cause with either API. See the README for migration examples.
