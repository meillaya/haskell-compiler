# PRD: WYAH Compiler Learning Roadmap

## Goals
- Implement Stephen Diehl's *Write You a Haskell* compiler path in Haskell as a serious learning exercise.
- Produce a faithful Milestone 1 typed interpreter / REPL pipeline before backend work.
- Preserve semantic fidelity to the book while allowing structural simplifications that improve pedagogy and verification.
- Use Stack and keep dependencies minimal unless clearly justified.

## Non-goals
- LLVM / native code generation in Milestone 1
- Optimizer pipeline in Milestone 1
- Typeclasses initially
- Advanced algebraic datatypes / full pattern-match compilation beyond the minimum learning path
- Layout-sensitive parsing as an initial requirement
- Exact module-for-module fidelity to the book
- Full Haskell language coverage
- Premature extensions unrelated to parser/typechecker/evaluator correctness

## User / Learning Value
- Learn compiler construction through explicit passes: parsing, binding, renaming, inference, desugaring, evaluation.
- Learn how Haskell supports large, compositional compiler implementations.
- Build a foundation for later extensions into richer frontends and LLVM.

## Constraints
- Must use Stack.
- Avoid external libraries when reasonably possible.
- Milestone 1 correctness means book-faithful semantics validated by broad regression tests.
- Structural deviations are allowed only when semantics remain faithful and the deviation is recorded.

## Deliverables
### Tier 1 — immediate user-facing outputs
- `.omx/plans/study-guide-wyah-milestone-1.md`
- `.omx/plans/roadmap-wyah-build-order.md`

### Tier 2 — mandatory Milestone 0 companions
- `.omx/plans/prd-wyah-compiler-learning-roadmap.md`
- `.omx/plans/test-spec-wyah-compiler-learning-roadmap.md`
- `.omx/plans/architecture-wyah-milestone-1.md`
- `.omx/plans/regression-matrix-wyah.md`
- `.omx/plans/parser-decision-wyah.md`
- `.omx/plans/diagnostics-policy-wyah.md`
- `.omx/plans/milestones-wyah.md`

## Milestones
### Milestone 0 — Planning + learning artifacts
Study guide, roadmap, PRD, test spec, architecture notes, regression matrix, parser decision, diagnostics policy, milestone tracker.

### Milestone 1 — Typed interpreter / REPL only
Stack scaffolding -> source syntax + AST -> parser -> pretty-printing / fixtures -> renamer -> type representation + substitutions -> HM inference -> typed core/desugaring -> evaluator -> REPL -> regression suite.

### Milestone 2 — Richer language surface
Parser ergonomics, minimal datatypes, broader pattern handling, better errors/source spans.

### Milestone 3 — Advanced front-end and semantics
More ProtoHaskell fidelity, selective typeclass groundwork, stronger IR boundaries.

### Milestone 4 — Backend preparation
Formalize Core/IR invariants and runtime/value representation choices.

### Milestone 5 — LLVM / native codegen
Backend implementation, codegen tests, runtime integration.

## Acceptance Criteria
- Milestone 1 is explicitly defined as typed interpreter / REPL only.
- All Tier 1 and Tier 2 Milestone 0 artifacts exist.
- Stack requirement and low-dependency bias are preserved in every artifact.
- Milestone 1 non-goals are preserved in every artifact.
- Correctness is defined as book-faithful semantics validated by broad regression tests.
- Deviation policy is recorded and distinguishes semantics from structure.

## Deviation Policy Summary
- **Binding:** semantics of scoping, inference, evaluation, and core forms.
- **Flexible:** module layout, helper abstractions, parser architecture, chapter-to-build order.
- Any semantic-impacting deviation must be logged with rationale and preserved behavior.
