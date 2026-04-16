# Milestones: WYAH Compiler Project

## Milestone 0
**Focus:** planning + learning artifacts.
**Dependencies:** deep-interview spec.
**Exit criteria:** all canonical planning artifacts exist and satisfy their checklists.

## Milestone 1
**Focus:** a verified typed interpreter / REPL baseline.
**Dependencies:** Milestone 0 complete.
**Current verified baseline includes:**
- spans and normalized diagnostics
- expressions: ints, bools, vars, lambdas, application, let, if
- builtins: `succ`, `pred`, `iszero`, `add`
- surface-only operator layer: infix `+`, prefix `-`
- minimal semicolon-separated top-level declarations
- mandatory final expression
- sequential, non-recursive declaration visibility
- grouped fixture-driven verification across parse/rename/infer/eval/program flows

**Current non-goals preserved:** LLVM, optimizer, typeclasses, advanced ADTs, layout-sensitive parsing, modules/imports, recursive declaration groups, full Haskell.

**Checkpoint status:** current baseline is consolidated and fully verified.

## Milestone 2
**Focus:** the next post-consolidation semantic/syntax family.
**Entry rule:** must be chosen by fresh planning from the current verified baseline.
**Likely candidates:** deeper declaration semantics or a broader grammar family, depending on the latest plan.

## Milestone 3
**Focus:** richer surface language and parser ergonomics after the next family is complete and verified.

## Milestone 4
**Focus:** advanced front-end semantics.

## Milestone 5
**Focus:** backend preparation, then eventual LLVM / native code generation.
