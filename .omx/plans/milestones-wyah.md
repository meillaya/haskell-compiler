# Milestones: WYAH Compiler Project

## Milestone 0
**Focus:** planning + learning artifacts.
**Dependencies:** deep-interview spec.
**Exit criteria:** all canonical `.omx/plans/` artifacts exist and satisfy checklists.

## Milestone 1
**Focus:** typed interpreter / REPL.
**Dependencies:** Milestone 0 complete.
**Exit criteria:** parser, renamer, HM inference, typed core, evaluator, REPL, and active-now regressions implemented and passing.
**Non-goals preserved:** LLVM, optimizer, typeclasses, advanced ADTs, layout-sensitive parsing, full Haskell.

## Milestone 2
**Focus:** richer surface language and parser ergonomics.
**Dependencies:** Milestone 1 semantics stable.
**Exit criteria:** staged parser/datatype work promoted deliberately.

## Milestone 3
**Focus:** advanced front-end semantics.
**Dependencies:** Milestone 2 boundaries stable.
**Exit criteria:** richer ProtoHaskell features and stronger IR seams.

## Milestone 4
**Focus:** backend preparation.
**Dependencies:** trusted core semantics.
**Exit criteria:** runtime/value representation and backend strategy are explicit.

## Milestone 5
**Focus:** LLVM / native codegen.
**Dependencies:** Milestone 4 complete.
**Exit criteria:** backend artifacts, codegen tests, runtime integration.
