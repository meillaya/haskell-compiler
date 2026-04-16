# Test Specification: WYAH Compiler Learning Roadmap

## Test Categories
- Parser acceptance
- Renamer / scope resolution
- Hindley–Milner inference
- Evaluator semantics
- Negative tests (unbound names, ill-typed terms)
- Forward-looking compatibility markers

## Chapter Mapping
| Chapter / Area | Primary Pass | Example Source | Status Expectation |
| --- | --- | --- | --- |
| Parsing | Parser | arithmetic / lambda parser examples | active-now |
| Lambda Calculus | Parser + Eval | substitution/reduction examples | active-now |
| Type Systems | Infer + Eval | STLC examples | active-now |
| Hindley-Milner Inference | Infer | polymorphism / occurs-check examples | active-now |
| Design of ProtoHaskell | Core + Eval | typed interpreter behaviors | active-now |
| Frontend / Renamer | Name | scope and binding examples | active-now |
| Datatypes | Future surface/core | datatype examples | staged-for-later or blocked-by-non-goal |
| LLVM / Codegen | Backend | codegen examples | blocked-by-non-goal |

## Negative Tests
- Unbound variable should fail in renamer with source location.
- Duplicate binding should fail in renamer with source location.
- Type mismatch should fail in inference with source location.
- Infinite type / occurs-check failure should be reported.
- Invalid syntax should fail in parser with line/column or equivalent span.

## Forward-Looking Tests
Each forward-looking test must be classified as exactly one of:
- `active-now`
- `staged-for-later`
- `blocked-by-non-goal`

### Policy
- `active-now`: within Milestone 1 scope and valuable for semantic drift prevention now.
- `staged-for-later`: documented now, not required to pass in Milestone 1.
- `blocked-by-non-goal`: explicitly out of Milestone 1 because it depends on a locked non-goal.

## Coverage Minimums
For plan readiness, the regression matrix must identify at least:
- 3 parser examples/tests
- 2 renamer/scope tests
- 3 HM inference / type-error tests
- 2 evaluator-semantics tests
- 4 forward-looking later-chapter markers total, split across staged-for-later / blocked-by-non-goal
- 12 total entries with at least 8 active-now, 2 staged-for-later, 2 blocked-by-non-goal

## Verification Commands
- `stack build`
- `stack test`
- `stack ghci`

## Evidence Expectations
- Required plan artifacts exist at the canonical paths.
- Required section headers exist in each artifact.
- No artifact widens Milestone 1 beyond locked non-goals.
- Regression matrix includes source chapter, expected semantics, owning stage, and tri-state status.
- Later-chapter tests do not silently expand present implementation scope.
