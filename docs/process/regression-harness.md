# Regression Harness

This project uses a fixture-driven regression harness to keep semantic claims attached to executable evidence.

## Purpose

The harness exists to prevent semantic drift while the typed interpreter grows. It is deliberately lightweight, text-based, and designed to remain compatible with the current expression language while leaving room for future whole-program tests.

## Canonical fixture schema

Each fixture record should support the following fields.

| Field | Required | Meaning |
| --- | --- | --- |
| `fixtureId` | yes | Stable unique identifier for the case |
| `sourceKind` | yes | `expr` for current inputs, `program` for future declaration/program inputs |
| `phase` | yes | Primary owning phase: `parse`, `rename`, `infer`, `eval`, or `pipeline` |
| `input` | yes | Source text under test |
| `expectationKind` | yes | Expected outcome kind |
| `expected` | conditional | Expected value, type, or normalized diagnostic payload |
| `status` | yes | `active-now`, `staged-for-later`, or `blocked-by-non-goal` |
| `chapterRef` | no | WYAH chapter or section reference |
| `notes` | no | Short provenance or rationale |

## Expectation kinds

The fixture system is intended to support these canonical expectation kinds:

- `parse-ok`
- `parse-error`
- `rename-ok`
- `rename-error`
- `infer-type`
- `infer-error`
- `eval-value`
- `pipeline-error`

The current implementation still uses a smaller TSV runner, but all future fixture expansion should move toward these canonical meanings.

## Phase ownership

### `parse`
Owns syntax acceptance/rejection and span-bearing parse diagnostics.

### `rename`
Owns lexical scope, binding identity, and unbound-name failures.

### `infer`
Owns type inference success, unification failures, occurs checks, and branch/type mismatches.

### `eval`
Owns runtime value expectations for well-typed programs.

### `pipeline`
Owns full-path failures and success cases that intentionally traverse multiple passes.

## Forward-compatible input model

The harness is designed around two input kinds:

- `expr` — active now
- `program` — reserved for the next major feature target (top-level declarations)

Current fixture work must not assume every test will always remain expression-shaped.

## Current implementation status

The active harness today consists of:

- grouped Haskell-side test execution in `test/WYAH/TestCases.hs`
- an initial external fixture corpus under `test/fixtures/`
- grouped reporting by parsing, renaming, inference, and evaluation

## Exit-gate role

This harness hardening pass is complete when:

1. canonical schema and policy docs exist,
2. external fixtures cover the current implemented language/features,
3. success and failure paths are represented across phases,
4. `stack build` and `stack test` pass,
5. README/docs explain the harness,
6. no declaration semantics have been implemented yet.

After that, the intended next target is **top-level declarations**.
