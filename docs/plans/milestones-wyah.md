# Milestones: WYAH Compiler Project

## Milestone 0
**Focus:** planning + learning artifacts.
**Dependencies:** deep-interview spec.
**Exit criteria:** all canonical planning artifacts exist and satisfy their checklists.

## Milestone 1
**Focus:** typed interpreter / REPL with explicit program structure and fixture-backed verification.
**Dependencies:** Milestone 0 complete.
**Current baseline includes:**
- spans and normalized diagnostics
- expressions: ints, bools, vars, lambdas, application, let, if
- builtins: `succ`, `pred`, `iszero`, `add`
- surface-only operator layer: infix `+`, prefix `-`
- minimal semicolon-separated top-level declarations
- grouped fixture-driven verification across parse/rename/infer/eval/program flows

**Exit criteria:**
- parser, renamer, HM inference, typed core, evaluator, and REPL remain green under fixture-backed verification
- current program/declaration contract is documented and enforced
- regression docs/fixtures are current
- remaining Milestone 1 gaps are explicitly identified before any Milestone 2 expansion

**Non-goals preserved:** LLVM, optimizer, typeclasses, advanced ADTs, layout-sensitive parsing, full Haskell, modules/imports, recursive declaration groups.

## Milestone 2
**Focus:** richer surface language and parser ergonomics.
**Dependencies:** Milestone 1 semantics stable and verified.
**Candidate work:** richer operator grammar, more declarations, improved parser ergonomics, selective datatype groundwork.

## Milestone 3
**Focus:** advanced front-end semantics.
**Dependencies:** Milestone 2 boundaries stable.
**Candidate work:** richer ProtoHaskell features, stronger IR seams, broader whole-program semantics.

## Milestone 4
**Focus:** backend preparation.
**Dependencies:** trusted core semantics.
**Exit criteria:** runtime/value representation and backend strategy are explicit.

## Milestone 5
**Focus:** LLVM / native codegen.
**Dependencies:** Milestone 4 complete.
**Exit criteria:** backend artifacts, codegen tests, runtime integration.
