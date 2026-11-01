# Deep Interview Spec: WYAH Compiler Learning Roadmap

> **Historical document.**  This records the original greenfield interview
> that produced the planning artifacts.  It is preserved for provenance, not
> as a current status report.  The current implementation and boundaries are
> described in `README.md`, `docs/language/protohaskell.md`, and
> `docs/audit/2026-11-01-audit.md`.

## Metadata

- Profile: standard
- Rounds: 7
- Final ambiguity: 0.17 (threshold 0.20)
- Context type: greenfield

## Intent

Implement the Write You a Haskell compiler in Haskell as a high-value learning
exercise spanning compiler theory, theory of computation, automata-adjacent
foundations, and Haskell idioms.

## Desired outcome

1. A compact study guide.
2. A read-in-build-order roadmap.
3. Milestone 1 as a faithful, testable typed interpreter/REPL.
4. A path to later surface-language, datatype, typeclass, and backend work.

## Non-goals for Milestone 1

LLVM/native codegen, optimizer, typeclasses initially, advanced ADTs/pattern
compilation beyond the minimum, layout-sensitive parsing initially, exact
module-for-module fidelity, full Haskell coverage.

## Decision boundaries

OMX could choose architecture, order, tests, examples, simplified syntax, and
book-structure deviations that improved pedagogy/correctness; it had to
preserve Stack and a low-dependency bias.

## Resolution

The original Milestone 1 baseline was built, audited, and then superseded by
the complete interpreted ProtoHaskell pipeline documented in the repository.
The book's LLVM chapter remains a tutorial stub, not a ProtoHaskell backend.
