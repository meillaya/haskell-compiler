# Regression Harness

This project uses a fixture-driven regression harness to keep semantic claims attached to executable evidence.

## Purpose

The harness exists to prevent semantic drift while the typed interpreter grows. It is deliberately lightweight, text-based, and compatible with both expression-shaped and program-shaped inputs.

## Canonical fixture schema

Each fixture record should support the following fields.

| Field | Required | Meaning |
| --- | --- | --- |
| `fixtureId` | yes | Stable unique identifier for the case |
| `sourceKind` | yes | `expr` or `program` |
| `phase` | yes | Primary owning phase: `parse`, `rename`, `infer`, `eval`, or `pipeline` |
| `input` | yes | Source text under test |
| `expectationKind` | yes | Expected outcome kind |
| `expected` | conditional | Expected value, type, or normalized diagnostic payload |
| `status` | yes | `active-now`, `staged-for-later`, or `blocked-by-non-goal` |
| `chapterRef` | no | WYAH chapter or section reference |
| `notes` | no | Short provenance or rationale |

## Expectation kinds

The current harness recognizes these canonical meanings:

- `parse-ok`
- `parse-error`
- `rename-error`
- `infer-type`
- `infer-error`
- `eval-value`
- `pipeline-error`

## Phase ownership

### `parse`
Owns syntax acceptance/rejection and normalized parse diagnostics.

### `rename`
Owns lexical scope, declaration visibility, binding identity, duplicate-name policy, and unbound-name failures.

### `infer`
Owns type inference success, unification failures, occurs checks, and branch/type mismatches.

### `eval`
Owns runtime value expectations for well-typed inputs.

### `pipeline`
Owns full-path failures and success cases that intentionally traverse multiple passes.

## Current fixture layout

The external fixture corpus is currently split by ownership:

- `test/fixtures/parser.tsv`
- `test/fixtures/rename.tsv`
- `test/fixtures/infer.tsv`
- `test/fixtures/eval.tsv`
- `test/fixtures/program.tsv`

## Current implementation status

The active harness today consists of:

- grouped Haskell-side test execution in `test/WYAH/TestCases.hs`
- phase-split external fixtures under `test/fixtures/`
- grouped reporting by parsing, renaming, inference, evaluation, and pipeline failures
- both `expr` and `program` source kinds

## Verification role

This harness is now the canonical verification surface for the current Milestone 1 baseline. It should be updated whenever semantics, diagnostics, or parser surface behavior change.
