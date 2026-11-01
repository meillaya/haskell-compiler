# PRD: WYAH Compiler Learning Roadmap

## Goals

- Implement Stephen Diehl's *Write You a Haskell* ProtoHaskell path in Haskell
  as a serious learning exercise.
- Preserve the book's semantic pipeline: parse, rename, kind-check, infer,
  desugar, elaborate to Core, evaluate.
- Keep the implementation readable, explicit, and testable with a minimal
  dependency surface.
- Document deviations and boundaries rather than silently widening or
  narrowing scope.

## Non-goals

- LLVM/native code generation or the later Fun pipeline.
- Optimizer, STG, Imp, or Core type checker.
- Modules/imports, full GADT type system, polymorphic recursion, defaulting,
  overloaded literals, default class methods, `deriving`, newtypes, type
  families, or GHC extensions.
- Exact module-for-module fidelity to the book.

## User / learning value

- See each pass's input/output and invariants.
- See how HM constraint generation, generalization, and dictionary elaboration
  fit together.
- See how layout, fixity, patterns, and desugaring are implemented without a
  parser generator.
- Use the REPL and `:core`/`:type` to inspect intermediate forms.

## Constraints

- Stack remains supported; Cabal is the verified build path for this baseline.
- Prefer `base` plus boot libraries; no parser generators or lens/effect
  libraries.
- Correctness means book-faithful semantics validated by regression tests.

## Deliverables

### Code

- `src/WYAH/` — complete interpreted ProtoHaskell pipeline.
- `app/Main.hs` — REPL and file runner.
- `test/` — accumulating runner, inline structural cases, six fixture
  files.
- `examples/` — runnable programs.

### Planning and process

- `docs/plans/prd-wyah-compiler-learning-roadmap.md` (this file)
- `docs/plans/roadmap-wyah-build-order.md`
- `docs/plans/architecture-wyah-milestone-1.md`
- `docs/plans/milestones-wyah.md`
- `docs/plans/regression-matrix-wyah.md`
- `docs/plans/parser-decision-wyah.md`
- `docs/plans/diagnostics-policy-wyah.md`
- `docs/plans/test-spec-wyah-compiler-learning-roadmap.md`
- `docs/plans/study-guide-wyah-milestone-1.md`
- `docs/process/declaration-semantics.md`
- `docs/process/declaration-annotations.md`
- `docs/process/regression-harness.md`
- `docs/process/diagnostics-assertion-policy.md`
- `docs/language/protohaskell.md`
- `docs/audit/2026-11-01-audit.md`

## Acceptance criteria

- `cabal build all` succeeds.
- `cabal test` passes all active cases.
- The compiler parses, renames, kind-checks, infers, elaborates, and evaluates
  the supported ProtoHaskell surface.
- The REPL and file runner work.
- The regression suite covers parser, renamer, kinds, inference, evaluation,
  programs, and classes, including negative cases.
- Boundaries are documented in README and the language reference.

## Deviation policy

- **Binding:** scoping, inference, desugaring, evaluation semantics, Core
  boundaries.
- **Flexible:** module layout, parser implementation, pretty-printing, test
  structure, chapter ordering.
- Semantic-impacting deviations are recorded in the audit/language reference.
