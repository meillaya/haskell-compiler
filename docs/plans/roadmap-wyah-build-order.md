# Build-Order Roadmap: WYAH

## Why this order differs from the book
The book is written to teach concepts; this roadmap is reordered to minimize dependency thrash and maximize semantic traceability.

## Recommended Read / Build Order
| Order | Chapter / Area | Implementation Phase | Test Sources | Defer / Unlock |
| --- | --- | --- | --- | --- |
| 1 | Concepts | Milestone 0 framing | pipeline concepts, terminology | unlocks architecture docs |
| 2 | Haskell Basics | Milestone 0 environment discipline | small Haskell support snippets | unlocks Stack + code organization |
| 3 | Parsing | Milestone 1 parser baseline | parsing examples, syntax failures | unlocks AST + fixtures |
| 4 | Lambda Calculus | Milestone 1 semantic core | substitution/reduction examples | unlocks evaluator semantics |
| 5 | Type Systems | Milestone 1 typed discipline | STLC examples and failures | unlocks typed core expectations |
| 6 | Evaluation | Milestone 1 evaluator design | call-by-* examples, interpreter choices | unlocks REPL behavior |
| 7 | Hindley-Milner Inference | Milestone 1 inference core | polymorphism, unification, occurs-check tests | unlocks principal-type behavior |
| 8 | Design of ProtoHaskell | Milestone 1 architectural framing | ProtoHaskell examples | unlocks pass boundaries |
| 9 | Engineering Overview | Milestone 0/1 module layout | module/pipeline mapping | unlocks architecture notes |
| 10 | Frontend | Milestone 1 AST + declarations | scope and syntax examples | unlocks richer source language |
| 11 | Renamer | Milestone 1 name resolution | scope tests | unlocks capture-free binding |
| 12 | Extended Parser | Milestone 2 | parser ergonomics tests | deferred |
| 13 | Datatypes | Milestone 2/3 | datatype examples | deferred |
| 14 | LLVM | Milestone 5 | backend examples | blocked by Milestone 1 non-goals |
| 15 | Code Generation (LLVM) | Milestone 5 | codegen examples | blocked by Milestone 1 non-goals |

## Dependency Graph
- Parser depends on syntax decisions and diagnostics policy.
- Renamer depends on surface AST.
- Inference depends on renamed AST and type representation.
- Core/desugaring depends on renamed + typed surface terms.
- Evaluator depends on typed core semantics.
- REPL depends on parser + renamer + infer + eval.
- Regression harness spans all prior passes.

## Immediate vs Deferred Reading
### Immediate
Concepts, Parsing, Lambda Calculus, Type Systems, Evaluation, Hindley-Milner, ProtoHaskell overview, Renamer.

### Deferred
Extended Parser, Datatypes, LLVM, full backend details.
