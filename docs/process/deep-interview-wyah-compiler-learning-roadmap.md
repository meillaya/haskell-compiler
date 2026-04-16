# Deep Interview Spec: WYAH Compiler Learning Roadmap

## Metadata
- Profile: standard
- Rounds: 7
- Final ambiguity: 0.17
- Threshold: 0.20
- Context type: greenfield
- Context snapshot: `.omx/context/wyah-compiler-learning-roadmap-20260416T013437Z.md`
- Transcript: `.omx/interviews/wyah-compiler-learning-roadmap-20260416T014403Z.md`

## Clarity Breakdown
| Dimension | Score | Notes |
| --- | --- | --- |
| Intent | 0.72 | Learn compiler theory + Haskell engineering through faithful reimplementation |
| Outcome | 0.82 | Immediate outputs are study guide + build-order roadmap; project milestone targets typed interpreter/REPL |
| Scope | 0.89 | Milestone 1 is deliberately narrow and backend-free |
| Constraints | 0.78 | Stack required; avoid external libraries where reasonable; pedagogy/correctness outrank book-structure fidelity |
| Success | 0.82 | Broad, book-derived regression suite; selected later-chapter tests for forward compatibility |

## Intent
Implement the Write You a Haskell compiler in Haskell as a high-value learning exercise spanning compiler theory, theory of computation, automata-adjacent foundations, and the Haskell idioms/quirks that make this scale of implementation educational.

## Desired Outcome
1. Produce a compact study guide for the book/project.
2. Produce a read-in-build-order roadmap for implementation.
3. Plan and execute milestone 1 as a faithful, testable typed interpreter / REPL pipeline.
4. Preserve a path to later milestones such as richer frontends, datatypes, typeclasses, and LLVM/native codegen.

## In Scope
- Reorganizing chapter material into a better implementation sequence.
- Choosing architecture and module layout that maximize pedagogy and correctness.
- Adding tests and example programs.
- Simplifying syntax or internal structure when it clarifies the learning path while preserving semantics.
- Building a regression harness from examples/exercises in the text, with some forward-looking tests from later chapters.

## Out of Scope / Non-goals for Milestone 1
- LLVM / native code generation
- Optimizer pipeline
- Typeclasses initially
- Algebraic datatypes and advanced pattern-match compilation beyond the minimum needed learning path
- Layout-sensitive parsing as an initial requirement
- Exact module-for-module fidelity to the book
- Full Haskell language coverage
- Premature extensions unrelated to parser/typechecker/evaluator correctness

## Decision Boundaries
OMX may decide without confirmation:
- chapter reordering / implementation order
- Stack project structure
- test strategy and example corpus
- simplified syntax choices
- deviations from the book’s exact structure when they improve pedagogy or correctness

OMX must preserve:
- use of Stack
- a strong bias against external libraries unless clearly justified

## Constraints
- Prefer standard/base libraries where practical.
- Milestone 1 correctness is semantic fidelity, not just demo success.
- Plan should keep later extensions feasible without overengineering milestone 1.

## Testable Acceptance Criteria
- Study guide covers the main conceptual sequence needed for the project.
- Build-order roadmap defines milestone sequencing and rationale.
- Milestone 1 plan reaches parser -> AST -> renamer -> HM inference -> desugared typed core -> evaluator/REPL.
- Regression suite is derived broadly from the text and includes negative tests for scope/type failures.
- Selected tests from later chapters are identified as future-facing compatibility markers.

## Assumptions Exposed + Resolutions
- "Correct" initially sounded like functional adequacy; resolved instead to book-faithful semantics backed by broad regression tests.
- A narrow implementation milestone does not imply narrow validation; validation should look beyond the immediate feature set.

## Pressure-pass Findings
The strongest clarified change was the shift from a possibly lightweight success bar to an explicit demand for broad, text-derived regression coverage and semantic faithfulness.

## Technical Context Findings
- Current repo is effectively greenfield for code: only `docs/WYAH.pdf` and `.omx/` metadata are present.
- Planning should assume Stack-based project scaffolding will be created later.
