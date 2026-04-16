# haskell-compiler

A pedagogical Haskell compiler project inspired by **Stephen Diehl's _Write You a Haskell_**. The goal is not to clone the book line-for-line, but to reimplement its core ideas in a way that keeps the **theory**, **architecture**, and **implementation tradeoffs** explicit.

Right now the project is in a **verified typed-interpreter baseline** stage. The current emphasis is still semantic clarity over backend work: parsing, name resolution, Hindley–Milner inference, desugaring, evaluation, diagnostics, and bounded whole-program structure are in place before any LLVM/codegen work is attempted.

## Project goals

This repository exists as a learning vehicle for:

- compiler construction from first principles;
- the theory of syntax, binding, static semantics, and evaluation;
- how Haskell's algebraic data types, pattern matching, immutability, and recursion support compiler implementation;
- how to grow a language in staged milestones without losing semantic discipline.

The current plan and process history live under `docs/`, especially:

- `docs/plans/prd-wyah-compiler-learning-roadmap.md`
- `docs/plans/roadmap-wyah-build-order.md`
- `docs/plans/architecture-wyah-milestone-1.md`
- `docs/plans/regression-matrix-wyah.md`
- `docs/process/declaration-semantics.md`
- `docs/process/declaration-annotations.md`
- `docs/process/regression-harness.md`
- `docs/process/diagnostics-assertion-policy.md`

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
- a minimal semicolon-separated top-level declaration form,
- builtin primitive names `succ`, `pred`, `iszero`, and `add`,
- a surface-only operator layer: infix `+` and prefix `-`.

This stage corresponds to the usual front-end syntax phase of a compiler. In theoretical terms, we are turning source text into a structured syntax tree with source locations. The implementation remains a mostly handwritten lexer/parser, which fits the current milestone’s low-dependency learning goals.

Example forms accepted today:

```haskell
42
true
\x -> x
(\x -> x) 42
let id = \x -> x in id 42
if false then 1 else 0
id = \x -> x; id 42
1 + succ 2
- 1
```

### 2. Names, scope, and binding

Once a program is parsed, it is renamed. Renaming is where syntax starts becoming semantics. A variable name in source code is only a string; after renaming it becomes a uniquely resolved binder reference.

The renamer currently:

- assigns unique IDs to binders,
- resolves variable occurrences against the nearest binder,
- rejects unbound variables,
- preserves source spans for diagnostics,
- enforces top-level declaration rules such as duplicate-name rejection and sequential visibility.

This reflects the theory of lexical scope, alpha-equivalence, and environment-based resolution.

### 3. Hindley–Milner type inference

The current type system already exercises the core ideas needed for Milestone 1:

- type variables,
- function types,
- unification,
- occurs checks,
- let-generalization,
- let-instantiation.

This is an Algorithm W-style baseline. It supports polymorphic `let` bindings and checks current programs through the same inference machinery used for expressions.

### 4. Program structure and lowering

The compiler now has an explicit `Program` / `Decl` model. A program is currently:

- an ordered list of top-level declarations,
- followed by a required final expression.

The important architectural invariant is that program shape is validated and renamed **before** lowering into expression-level structure. The current lowering step then desugars declarations into nested `let`s for the existing interpreter pipeline.

### 5. Evaluation

Evaluation is interpreter-based. That is deliberate: the project is still proving the semantics of the front-end before generating machine code.

The evaluator currently supports:

- integers,
- booleans,
- closures,
- application,
- `let`,
- `if`,
- predefined primitives,
- programs lowered from top-level declarations.

### 6. Diagnostics and source spans

A serious compiler must explain failure precisely. The project now has a normalized diagnostic contract across parser, renamer, inferencer, and evaluator:

```text
phase=<phase>; source=<source>; span=<span>; message=<message>
```

This is intentionally structural and testable. It supports the current fixture harness without overcommitting to a polished UX layer too early.

## Architecture so far

The codebase remains intentionally small and pass-oriented.

```text
app/
  Main.hs
src/WYAH/
  Diagnostic.hs
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
  WYAH/TestCases.hs
  fixtures/
```

### `WYAH.Diagnostic`
Provides the normalized phase/source/span/message rendering contract.

### `WYAH.Span`
Tracks source positions and spans.

### `WYAH.Syntax`
Defines surface syntax, declarations, and program structure.

### `WYAH.Parser`
Implements tokenization and parsing for both expressions and current program-shaped inputs.

### `WYAH.Name`
Implements renaming, top-level declaration checks, and renamed program lowering.

### `WYAH.Type`
Defines the current type language: `Int`, `Bool`, variables, and function types.

### `WYAH.Infer`
Implements Hindley–Milner inference and current program-level checking through lowered forms.

### `WYAH.Core`
Defines the smaller internal expression language used after renaming.

### `WYAH.Eval`
Evaluates the Core language using closures and predefined runtime bindings.

### `WYAH.Repl`
Connects parsing, renaming, inference, lowering, evaluation, and output.

## Implementation details achieved so far

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
1 + 2
- 1
id = \x -> x; keep = id 42; id keep
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
- semicolon-separated top-level declarations are first-class through parsing/renaming and then lower into expression-level structure only after program validation;
- top-level declarations are sequential, non-recursive, and require a final expression;
- let-polymorphism generalizes the bound type scheme and instantiates it at later use sites;
- a predefined environment provides arithmetic and predicate primitives;
- the operator layer is surface-only and desugars to existing builtin/application semantics.

### Example REPL interaction

```text
$ stack exec wyah-compiler
WYAH scaffold REPL (:quit to exit)
> let id = \x -> x in id 42
type: Int
value: VInt 42
> id = \x -> x; keep = id 42; id keep
type: Int
value: VInt 42
> 1 + succ 2
type: Int
value: VInt 4
> :quit
bye
```

## What has been achieved so far

The project has completed a significant Milestone-1 baseline:

- planning artifacts and milestone docs,
- Stack scaffold,
- spans and normalized diagnostics,
- expression semantics,
- let-polymorphism,
- booleans and conditionals,
- builtins,
- a fixture-driven regression harness,
- a minimal program/declaration model,
- a surface-only operator layer,
- a consolidation pass refreshing docs and regression tracking.

## Verification model

The current regression harness is canonical.

It combines:
- a few inline structural tests for parser-span and rename-location checks;
- grouped external fixtures under `test/fixtures/` for parser, rename, infer, eval, and program-level behavior.

The harness now covers both `expr` and `program` source kinds and is the primary source of semantic confidence for the current baseline.

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

### Expression-mode smoke test

```bash
printf '1 + succ 2\n:quit\n' | stack exec wyah-compiler
```

### Program-mode smoke test

```bash
printf 'id = \\x -> x; keep = id 42; id keep\n:quit\n' | stack exec wyah-compiler
```

## Current limitations

The implementation is still intentionally narrow.

Not implemented yet:

- optional top-level declaration type annotations,
- recursive or mutually recursive top-level declarations,
- modules/imports,
- pattern matching,
- algebraic datatypes,
- typeclasses,
- layout-sensitive parsing,
- rich multi-span diagnostics,
- optimization passes,
- LLVM or native code generation.

This is still a semantics-first baseline, not a production language.

## Current checkpoint

The repository is currently at a **verified typed-interpreter baseline**.

That baseline includes:
- normalized diagnostics,
- fixture-driven verification,
- minimal whole-program support,
- and a small surface operator layer over existing semantics.

The next semantic/syntax family is now the bounded slice of optional top-level declaration type annotations, as approved by planning.

## Documentation layout

Project-facing documentation lives in `docs/`:

- `docs/WYAH.pdf` — local copy of the reference text
- `docs/plans/` — roadmap, architecture, milestones, regression matrix, diagnostics policy, PRD
- `docs/process/` — declaration semantics, regression harness, diagnostics assertion policy, and planning/process artifacts

`.omx/` remains local runtime/orchestration state and is not canonical project documentation.

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
