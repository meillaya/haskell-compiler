# Regression Harness

The project uses an accumulating fixture-driven harness to keep semantic
claims attached to executable evidence.

## Runner

`test/Main.hs` runs every group and every case, prints `[ok]`/`[fail]` with
details, prints a summary, and exits nonzero only after all groups have run.
It never stops at the first failure.

## Fixture schema

Each non-comment row has nine pipe-separated fields:

```text
fixtureId|sourceKind|phase|expectationKind|status|input|expected|chapterRef|notes
```

| Field | Meaning |
| --- | --- |
| `fixtureId` | stable unique identifier |
| `sourceKind` | `expr` or `program` |
| `phase` | `parse`, `rename`, `kind`, `infer`, `eval`, `program`, or `pipeline` |
| `expectationKind` | expected outcome kind |
| `status` | `active-now` runs; `staged-for-later`/`blocked-by-non-goal` skip |
| `input` | source text; `\n`, `\t`, `\\`, and `\|` escapes are decoded |
| `expected` | type, value, span, or stable diagnostic fragment |
| `chapterRef` | book chapter/area |
| `notes` | provenance/rationale |

Malformed rows are errors; they are never silently dropped.

## Expectation kinds

- `parse-ok` / `parse-error` / `parse-span`
- `rename-error` / `kind-error` / `infer-error` / `eval-error`
- `infer-type`
- `eval-value`
- `program-type` / `program-value`

Assertions are structural where practical: phases, spans, type strings, and
rendered values.  Diagnostic assertions use stable fragments rather than full
prose.

## Fixture files

- `test/fixtures/parser.tsv`
- `test/fixtures/rename.tsv`
- `test/fixtures/infer.tsv`
- `test/fixtures/eval.tsv`
- `test/fixtures/program.tsv`
- `test/fixtures/classes.tsv`

Fixtures are declared as Cabal data files and are located by searching the
source/working-directory roots, so the suite does not depend on a single CWD.

## Inline cases

`test/WYAH/TestCases.hs` also contains structural cases for spans, layout,
left-nested applications, kinds, shadowing, recursion, annotations, and
runtime behavior.  These cover properties that do not fit a one-line TSV row.

## Verification commands

```bash
cabal build all
cabal test
stack test          # when Stack/GHC 9.6.7 is available
devenv test         # when a devenv shell is available
```
