# Architecture Notes: WYAH Milestone 1

## Proposed Stack Layout
- `app/` — CLI / REPL entrypoint later
- `src/Syntax.hs`
- `src/Parser.hs`
- `src/Name.hs`
- `src/Type.hs`
- `src/Infer.hs`
- `src/Core.hs`
- `src/Eval.hs`
- `src/Repl.hs`
- `test/` — regression and negative tests

## Canonical Pipeline / IR Table
| Stage | Output | Invariants | Owner |
| --- | --- | --- | --- |
| Source | raw text | preserved for diagnostics | Parser/Syntax |
| Parse | Surface AST | spans present; no name/type resolution | Parser |
| Rename | Renamed AST | unique binders; no accidental capture | Name |
| Infer | typed info / failures | fresh vars, lawful substitutions, principal-type intent | Infer/Type |
| Desugar | typed Core | semantics preserved; no unresolved names | Core |
| Evaluate | runtime value / error | behavior matches selected WYAH semantics | Eval |
| REPL | interactive pipeline result | stage-labeled errors; no hidden state corruption | Repl |

## Module Boundaries
- `Syntax`: surface/core syntax, names, literals, spans.
- `Parser`: parse surface syntax and attach spans.
- `Name`: resolve names, reject unbound/duplicate bindings.
- `Type` + `Infer`: HM machinery, substitutions, unification, generalization.
- `Core`: small typed IR and desugaring boundary.
- `Eval`: interpreter semantics for core terms.
- `Repl`: integrates the pipeline for iterative experimentation.

## Extension Seams
- richer parser and layout handling after Milestone 1
- datatype support after semantic core is trusted
- typeclass groundwork after stable inference/core boundaries
- LLVM only after runtime/value representation decisions are explicit
