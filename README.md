# haskell-compiler

A pedagogical Haskell compiler project inspired by **Stephen Diehl's _Write You a Haskell_**. The goal is not to clone the book line-for-line, but to reimplement its core ideas in a way that keeps the **theory**, **architecture**, and **implementation tradeoffs** explicit.

Right now the project is still in the **typed-interpreter stage** of the roadmap. That is deliberate. The current milestone is to make parsing, name resolution, Hindley–Milner inference, desugaring, evaluation, and diagnostics correct and explainable **before** introducing backend complexity like LLVM.

## Project goals

This repository exists as a learning vehicle for:

- compiler construction from first principles;
- the theory of syntax, binding, static semantics, and evaluation;
- how Haskell's algebraic data types, pattern matching, immutability, and recursion support compiler implementation;
- how to grow a language in staged milestones without losing semantic discipline.

The current plan is now documented in `docs/`, especially:

- `docs/plans/prd-wyah-compiler-learning-roadmap.md`
- `docs/plans/roadmap-wyah-build-order.md`
- `docs/plans/architecture-wyah-milestone-1.md`
- `docs/plans/regression-matrix-wyah.md`
- `docs/process/deep-interview-wyah-compiler-learning-roadmap.md`

## Theory in scope so far

### 1. Surface syntax and parsing

The parser currently handles a deliberately small language. At the moment that language includes:

- integer literals,
- boolean literals,
- variables,
- lambda abstractions,
- application,
- `let ... in ...`,
- `if ... then ... else ...`,
- builtin primitive names `succ`, `pred`, `iszero`, and `add`.

This stage corresponds to the usual front-end syntax phase of a compiler. In theoretical terms, we are turning a source string into a structured syntax tree with source locations. The implementation is currently a **minimal handwritten lexer/parser**, which fits the milestone constraint of avoiding external libraries when possible.

Example forms accepted today:

```haskell
42
true
\x -> x
(\x -> x) 42
let id = \x -> x in id 42
if false then 1 else 0
```

### 2. Names, scope, and binding

Once a program is parsed, it is renamed. Renaming is where syntax starts becoming semantics. A variable name in source code is only a string; after renaming it becomes a uniquely resolved binder reference.

This prevents accidental capture and makes later passes simpler. In practice, the renamer currently:

- assigns unique IDs to binders,
- resolves variable occurrences against the nearest binder,
- rejects unbound variables,
- preserves source spans for diagnostics.

This is the first place where the project directly reflects the theory of lexical scope and alpha-equivalence.

### 3. Hindley–Milner type inference

The current type system is still small, but it already exercises the most important ideas:

- type variables,
- function types,
- unification,
- occurs checks,
- let-generalization,
- let-instantiation.

In other words, this project already contains the beginning of an **Algorithm W-style** inference engine. The implementation currently supports polymorphic `let` bindings in the usual Hindley–Milner style.

For example, the following now works:

```haskell
let id = \x -> x in let keep = id 42 in id (\y -> y)
```

and infers a function type for the final expression.

### 4. Core language and desugaring boundary

The project separates the surface syntax from a smaller internal representation in `WYAH.Core`. This is the first real architectural seam of the compiler.

The purpose of the Core language is not optimization yet. Instead, it serves two more basic goals:

- make evaluation rules simpler and more explicit;
- ensure later phases do not depend on parser-oriented surface structure.

The current core still closely mirrors the surface language, but it already provides a clean place to evolve future semantic passes.

### 5. Evaluation

Evaluation is currently interpreter-based. That keeps the project aligned with the current milestone: prove the semantics of the front-end before generating machine code.

The evaluator currently supports:

- integers,
- booleans,
- closures,
- application,
- `let`,
- `if`.

This gives the project a complete vertical slice:

```text
source -> parse -> rename -> infer -> core -> evaluate
```

That full path is already executable through the REPL.

### 6. Diagnostics and source spans

A serious compiler needs more than success cases. It must explain failure precisely. The current implementation now tracks parser-level spans and uses them in diagnostics. Rename and inference errors also report locations.

That means failures like these are intentional and test-covered:

- malformed syntax;
- unbound variables;
- branch type mismatches;
- non-boolean conditional guards;
- occurs-check failures;
- attempts to apply non-functions.

This is still a minimal diagnostics system, not a polished one, but it already enforces an important compiler design rule: **error reporting is part of semantics work, not a cosmetic afterthought**.

## Architecture so far

The codebase is intentionally small and pass-oriented.

```text
app/
  Main.hs
src/WYAH/
  Span.hs
  Syntax.hs
  Parser.hs
  Name.hs
  Type.hs
  Infer.hs
  Core.hs
  Eval.hs
  Repl.hs
test/
  Main.hs
```

### `WYAH.Span`
Tracks source positions and spans. This is the shared location layer used by diagnostics.

### `WYAH.Syntax`
Defines the surface AST. This is the parsed language before names are resolved.

### `WYAH.Parser`
Implements tokenization and parsing. The parser attaches spans directly to syntax nodes and produces parse errors with file/position context.

### `WYAH.Name`
Implements renaming. This resolves lexical scope and gives each binder a unique identity.

### `WYAH.Type`
Defines the type language used by inference. Right now that includes `Int`, `Bool`, type variables, and function types.

### `WYAH.Infer`
Implements type inference and unification. This module currently contains the most theory-heavy logic in the project.

### `WYAH.Core`
Defines the smaller internal language used after renaming and inference.

### `WYAH.Eval`
Evaluates the Core language using closures and environments.

### `WYAH.Repl`
Wires everything together into the interactive driver.

## Implementation details achieved so far

The current implementation supports the following language fragment.

### Supported syntax

```haskell
42
true
false
\x -> x
(\x -> x) 42
let id = \x -> x in id 42
if true then 1 else 0
succ 0
add 2 3
```

### Supported types

```text
Int
Bool
t0 -> t0
t0 -> t1 -> t0
```

### Supported semantic behaviors

- lambda abstraction creates closures;
- application substitutes by environment, not by textual rewriting;
- `let` evaluates the bound expression and introduces a new scope;
- `if` chooses a branch only after checking a boolean guard;
- let-polymorphism generalizes the bound type scheme and instantiates it at later use sites;
- a small predefined environment provides arithmetic and predicate primitives without requiring operator syntax yet.

### Example REPL interaction

```text
$ stack exec wyah-compiler
WYAH scaffold REPL (:quit to exit)
> let id = \x -> x in id 42
type: Int
value: VInt 42
> if false then 1 else 0
type: Int
value: VInt 0
> :quit
bye
```

## What has been achieved so far

The project has already completed several important milestones inside the overall typed-interpreter roadmap.

### Planning and milestone definition

The initial Ralph / planning loop produced:

- a PRD,
- a test strategy,
- an architecture note,
- a regression matrix,
- a milestone tracker,
- a parser decision record,
- a diagnostics policy.

These documents now live under `docs/plans/` and `docs/process/`, while `.omx/` itself is treated as local agent/runtime state and ignored by git.

### Working Stack scaffold

The repository now builds as a Stack project with:

- one library,
- one executable,
- one test suite.

No third-party implementation dependencies have been introduced for the compiler itself so far. The repository-facing documentation is now kept in `docs/`, while `.omx/` remains local orchestration state.

### End-to-end pipeline

The following pipeline works today:

```text
Source text
  -> tokenization/parsing
  -> AST with spans
  -> renaming
  -> HM inference
  -> Core conversion
  -> evaluation
  -> REPL output
```

### Regression coverage currently implemented

The current executable tests are now organized as a small regression harness grouped by compiler phase, with an initial external fixture corpus under `test/fixtures/`. The harness currently covers:

- parsing of literals, lambdas, `let`, booleans, and conditionals;
- parse failures with file/span reporting;
- scope failures with source locations;
- inference of `id`, `const`, booleans, conditionals, and builtin primitives;
- occurs-check failure and branch/type mismatch failures;
- let-polymorphism reuse;
- evaluation of application, `let`, conditionals, and the predefined primitives `succ`, `pred`, `iszero`, and `add`;
- primitive-driven control-flow examples;
- ill-typed application rejected before evaluation.

## Verification model

The test harness now has two layers:

- a small set of inline structural tests for parser-span and rename-location details;
- an external fixture corpus under `test/fixtures/` for phase-owned inference/evaluation and broader regression coverage.

The fixture work is intentionally a **bounded hardening pass** before the next feature target. The intended next language feature after this verification pass remains **top-level declarations**.

## Build, run, and test

### Build

```bash
stack build
```

### Run tests

```bash
stack test
```

### Run the REPL

```bash
stack exec wyah-compiler
```

### Run a one-shot REPL smoke test from the shell

```bash
printf 'if false then 1 else 0\n:quit\n' | stack exec wyah-compiler
```

## Current limitations

The implementation is intentionally incomplete. The project is still inside a narrow, semantics-first milestone.

Not implemented yet:

- parser-level operator syntax (the project currently uses builtin names instead),
- top-level declarations/modules,
- pattern matching,
- algebraic datatypes,
- typeclasses,
- layout-sensitive parsing,
- richer error presentation,
- optimization passes,
- LLVM or native code generation.

This is a feature, not a bug. The current goal is to make the front-end and typed interpreter trustworthy before widening the language.

## Next major milestones

### Milestone 1 remainder: richer typed interpreter

The next substantial work items are:

- grow the current grouped regression harness into a larger chapter-derived corpus with externalized fixtures;
- add the next small but coherent language slices from the roadmap;
- improve diagnostic consistency across parse, rename, infer, and eval;
- keep extending the typed interpreter until it covers the approved Milestone 1 scope.

### Milestone 2: richer surface language

After the current milestone stabilizes, the project should move into:

- parser ergonomics,
- more declarations,
- minimal datatype support,
- better spans and diagnostics,
- more of the ProtoHaskell-like surface language.

### Milestone 3+: advanced front-end and backend preparation

Longer-term milestones include:

- stronger intermediate representations,
- broader semantic coverage,
- datatype and pattern compilation,
- eventual backend preparation,
- LLVM code generation only after the semantic core is trusted.

## Development notes

### Why use a handwritten parser first?

This is a deliberate learning tradeoff. A handwritten parser makes tokenization, precedence, and location tracking visible. Later milestones may justify Alex or Happy, but the current milestone values transparency over convenience.

### Why an interpreter before LLVM?

Because code generation is only worth doing once the source language semantics are stable. An interpreter gives immediate feedback on whether parsing, binding, inference, and control flow mean what they are supposed to mean.

### Why so much emphasis on tests now?

Because “compiler progress” is easy to fake with a few demos. A regression suite derived from the book prevents accidental semantic drift and keeps each new feature tied to an explicit claim.

## Documentation layout

Project-facing documentation lives in `docs/`:

- `docs/WYAH.pdf` — local copy of the reference text
- `docs/plans/` — roadmap, architecture, milestones, regression matrix, diagnostics policy, PRD
- `docs/process/` — captured deep-interview/spec/context artifacts that explain how the current direction was chosen

The `.omx/` directory still exists locally for agent runtime state, but it is now ignored by git and should not be treated as canonical project documentation.

## References

The following resources are the required foundation for implementing this project seriously from scratch.

- **Stephen Diehl, _Write You a Haskell_** — the main project inspiration and reference implementation path. Local copy: `docs/WYAH.pdf`. Online mirror: <https://smunix.github.io/dev.stephendiehl.com/fun/WYAH.pdf>
- **Robin Milner, “A Theory of Type Polymorphism in Programming” (1978)** — the classic reference for ML-style polymorphism and Algorithm W foundations. PDF mirror: <https://www.pure.ed.ac.uk/ws/files/15143545/1_s2.0_0022000078900144_main.pdf>
- **Luis Damas and Robin Milner, “Principal type-schemes for functional programs” (1982)** — the standard reference for principal types and let-polymorphism. Search-accessible copy: <https://www.researchgate.net/publication/215519844_Principal_type-schemes_for_functional_programs>
- **Benjamin C. Pierce, _Types and Programming Languages_** — the best single text for operational semantics, typing rules, and interpreter/typechecker design discipline. MIT Press: <https://mitpress.mit.edu/9780262303828/types-and-programming-languages/>
- **Simon Peyton Jones and David Lester, _Implementing Functional Languages: A Tutorial_** — invaluable for graph reduction, runtime models, and the path from semantics to implementation strategy. PDF mirror: <https://www.cse.iitb.ac.in/~as/fpcourse/PJ_DL_book.pdf>
- **GHC Commentary / GHC compiler documentation** — required to understand how a large production Haskell compiler organizes passes and intermediate languages. <https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler>
- **Alex User Guide** — needed when the project grows beyond the handwritten parser stage into generated lexers. <https://haskell-alex.readthedocs.io/>
- **Happy Documentation** — needed for later parser milestones if the grammar outgrows the current handwritten approach. <https://haskell-happy.readthedocs.io/>
- **Stephen Diehl, _What I Wish I Knew When Learning Haskell_** — useful companion for the Haskell techniques needed to keep the compiler implementation clean and idiomatic. <https://sdiehl.github.io/wiwinwlh/>

