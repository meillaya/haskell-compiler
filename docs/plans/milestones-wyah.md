# Milestones: WYAH Compiler Project

## Milestone 0 — Planning artifacts — complete

Study guide, roadmap, PRD, test spec, architecture, regression matrix, parser
decision, diagnostics policy, and milestone tracker exist under `docs/`.

## Milestone 1 — Verified typed interpreter baseline — complete, superseded

The original bounded interpreter baseline (ints/bools/lambdas/let/if,
builtins, minimal declarations) was verified, audited, and then replaced by the
full ProtoHaskell pipeline.  Its defects and remediation are recorded in
`docs/audit/2026-11-01-audit.md`.

## Milestone 2 — Richer surface language — complete

- layout-sensitive parser with explicit and implicit blocks;
- data declarations (regular, GADT syntax, records);
- type synonyms;
- classes, instances, superclasses, instance contexts;
- guards, pattern guards, `where`, as-patterns;
- lists, tuples, ranges, comprehensions, sections, custom fixities;
- comments and string/char escapes.

## Milestone 3 — Advanced frontend semantics — complete

- kind inference with higher-kinded parameters;
- canonical types/schemes/predicates;
- Hindley–Milner with let-polymorphism and occurs checks;
- qualified schemes, evidence ids, dictionary elaboration;
- skolemized signature checking;
- recursive top-level functions and local `letrec`/`where`.

## Milestone 4 — Backend preparation and interpreter — complete for ProtoHaskell

- explicit Core IR;
- function/case pattern-match compilation;
- dictionary and superclass elaboration;
- call-by-need interpreter with lazy lists and IO actions;
- REPL commands and file runner;
- fixture-driven regression harness.

## Milestone 5 — LLVM / native code generation — out of book scope

The book's LLVM chapter is a tutorial on LLVM IR and a toy x86-64 JIT; the
ProtoHaskell pipeline ends at the interpreter, and the native "Fun" pipeline
is explicitly later work.  No LLVM backend is claimed or implemented.
