# Regression Matrix: WYAH

| ID | Source Chapter | Example / Behavior | Expected Semantics | Owning Stage | Status |
| --- | --- | --- | --- | --- | --- |
| P1 | Parsing | integer literal parses | AST node created with span | Parser | active-now |
| P2 | Parsing | lambda parses | lambda AST shape preserved | Parser | active-now |
| P3 | Parsing | malformed token stream fails | parse error with location | Parser | active-now |
| N1 | Renamer | unbound variable `y` in `\x -> y` | reject with source location | Name | active-now |
| N2 | Renamer | duplicate binding / conflicting binder | reject with source location | Name | active-now |
| T1 | Type Systems / HM | `id` infers `forall a. a -> a` | principal type reported | Infer | active-now |
| T2 | HM | `const` infers `forall a b. a -> b -> a` | principal type reported | Infer | active-now |
| T3 | HM | `\x -> x x` fails occurs check | infinite type rejected | Infer | active-now |
| E1 | Evaluation | arithmetic / basic term evaluates | result matches selected semantics | Eval | active-now |
| E2 | Evaluation | ill-typed term never reaches eval | type failure reported earlier | Infer | active-now |
| F1 | Datatypes | minimal datatype constructor behavior | compatibility marker only | Core | staged-for-later |
| F2 | Extended Parser | richer operator/layout case | compatibility marker only | Parser | staged-for-later |
| B1 | LLVM | simple arithmetic codegen | blocked by Milestone 1 non-goal | Core/Backend | blocked-by-non-goal |
| B2 | Typeclasses | overloaded plus via dictionary passing | blocked by Milestone 1 non-goal | Infer/Core | blocked-by-non-goal |

## Notes
- `active-now` tests should inform Milestone 1 implementation immediately.
- `staged-for-later` tests shape architecture but do not gate Milestone 1 completion.
- `blocked-by-non-goal` tests document future direction and must not widen current scope.
