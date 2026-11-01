# wyah-compiler

A pedagogical implementation of the compiler path in Stephen Diehl's
**_Write You a Haskell_**.  The project follows the book's ProtoHaskell
language and pipeline, but it is an independent implementation with explicit
intermediate representations, a fixture-driven regression suite, and a small
dependency surface.

The compiler is intentionally a **semantics-first, interpreter-backed**
implementation.  It covers the complete ProtoHaskell language described in
the book's frontend chapters (8–17); the book's LLVM chapter is a tutorial for
a later, native "Fun" language and does not contain a ProtoHaskell backend.

```text
source
  -> lexer + layout
  -> parser                 (surface AST)
  -> renamer                (unique binders, grouped equations)
  -> desugar                (do notation, comprehensions)
  -> kind inference         (canonical types, type-class environments)
  -> Hindley–Milner         (constraint generation, generalization)
  -> elaboration            (dictionaries, pattern-match compilation)
  -> Core                   (small, evidence-explicit IR)
  -> lazy interpreter       (call-by-need thunks, IO actions)
```

## Status

The repository now implements a complete interpreted ProtoHaskell pipeline:

- layout-sensitive lexer and parser;
- data declarations in regular and GADT-style syntax, including records;
- type synonyms with expansion and saturation checks;
- single-parameter type classes, instances, superclasses, and instance
  contexts;
- type signatures (`::`, `forall`, and contexts), local signatures, and
  expression annotations;
- function equations with nested patterns, guards, pattern guards, `where`,
  and recursion;
- `let`, `if`, `case`, `do`, list/tuple/range syntax, list comprehensions,
  operator sections, and custom fixities;
- Hindley–Milner inference with let-polymorphism, occurs checks, qualified
  schemes, and dictionary-passing elaboration;
- lazy evaluation with recursive bindings and infinite lists;
- `IO` actions, `do` notation, and the book's `fix` combinator;
- a REPL with `:load`, `:reload`, `:browse`, `:type`, `:kind`, `:core`, `:source`,
  `:module`, and `:quit`;
- a file runner and a fixture-driven regression suite.

The current verified baseline is **146 passing regression cases** (inline
structural tests plus parser, renaming, kind, inference, evaluation, program,
and class fixtures).

## Try it

```bash
cabal build all
cabal test

printf '1 + 2 * 3\n:quit\n' | cabal run wyah-compiler
cabal run wyah-compiler -- examples/primes.fun
```

Expected file-runner output includes the inferred type and the evaluated
value:

```text
type: [Int]
value: [2, 3, 5, 7, 11, 13, 17, 19]
```

The REPL keeps declarations between lines and evaluates expressions in the
accumulated scope:

```text
ProtoHaskell Compiler 0.1.0 (:help for commands, :quit to exit)
> id x = x
id :: forall t0. t0 -> t0
> id 42
type: Int
value: 42
> :type same
forall t0. Eq2 t0 => t0 -> t0 -> Bool
```

## Language examples

### Algebraic data types and pattern matching

```haskell
data Maybe2 a = Nothing2 | Just2 a

fromMaybe d Nothing2   = d
fromMaybe d (Just2 x)  = x

main = fromMaybe 0 (Just2 42)
```

### Guards, where, and recursion

```haskell
sign x
  | x > 0     = 1
  | x < 0     = -1
  | otherwise = 0

sumTo n = go 0 1
  where
    go acc i
      | i > n     = acc
      | otherwise = go (acc + i) (i + 1)

main = sumTo 10
```

### Type classes and dictionaries

```haskell
class Eq2 a where
  eq2 :: a -> a -> Bool

instance Eq2 Int where
  eq2 x y = x == y

instance Eq2 a => Eq2 [a] where
  eq2 xs ys = case xs of
    []       -> case ys of
                  [] -> True
                  _  -> False
    (x:xs')  -> case ys of
                  []       -> False
                  (y:ys')  -> if eq2 x y then eq2 xs' ys' else False

same :: Eq2 a => a -> a -> Bool
same x y = eq2 x y

main = same [1,2,3] [1,2,3]
```

Elaboration turns the constraint into a dictionary parameter and selects the
instance dictionary at the use site.

### Lazy lists

```haskell
sieve [] = []
sieve (p:ps) = p : sieve (filter (nonMultiple p) ps)

nonMultiple p n = ((n / p) * p) /= n

primes = sieve [2..]

main = take 5 primes
```

### IO and do notation

```haskell
greet name = do
  putStr "Hello, "
  putStrLn name

main = do
  name <- getLine
  greet name
```

## Build and test

The project is tested with GHC 9.10.3 and declares support for GHC 9.6.7 and
9.10.3.

### Cabal

```bash
cabal build all
cabal test
cabal run wyah-compiler
```

### Stack

```bash
stack build
stack test
stack exec wyah-compiler
```

`stack.yaml` pins `ghc-9.6.7`.  The Cabal path is the one used for this
verified baseline.

### devenv

The repository includes an optional minimal devenv shell:

```bash
devenv shell
cabal build all
cabal test
```

`devenv.yaml` pins nixpkgs; `devenv.nix` supplies GHC, Cabal, and Git.  The
shell intentionally avoids a large `ghcWithPackages` closure; add Haskell
libraries to `cabal`'s `build-depends` instead.

## Architecture

```text
app/Main.hs              CLI: REPL or file runner
src/WYAH/
  Span.hs                source positions and spans
  Diagnostic.hs          normalized phase/source/span/message contract
  Syntax.hs              surface AST
  Lexer.hs               tokens, comments, layout (virtual braces/semicolons)
  Parser.hs              recursive-descent parser + fixity resolution
  Renamed.hs             renamed AST
  Renamer.hs             unique binders, recursive scopes, record selectors
  Type.hs                canonical types, kinds, schemes, predicates
  Prelude.hs             built-in names and schemes
  Kind.hs                synonym expansion, kind inference, data/class envs
  Typed.hs               inferred expression tree with evidence ids
  Infer.hs               Hindley–Milner + qualified schemes
  Core.hs                small Core language with dictionaries
  Desugar.hs             do notation and list comprehensions
  Elaborate.hs           dictionary elaboration + pattern-match compilation
  Eval.hs                call-by-need interpreter and IO primitives
  Repl.hs                compile pipeline, REPL, file runner
test/
  Main.hs                accumulating test runner
  WYAH/TestCases.hs      inline cases + fixture loader
  fixtures/              parser/rename/kind/infer/eval/program/classes TSVs
examples/                runnable ProtoHaskell programs
docs/
  audit/                 audit findings and remediation
  language/              language reference
  plans/                 roadmap, architecture, matrix, test spec
  process/               declaration semantics, harness, diagnostics policy
  WYAH.pdf               local reference copy
```

### Pass invariants

| Stage | Output | Invariant |
| --- | --- | --- |
| Lex/parse | surface AST | source spans on every node; no resolution |
| Rename | renamed AST | every binder unique; equations grouped; scopes explicit |
| Desugar | renamed AST | no `do`/comprehension sugar remains |
| Kind | canonical env | synonyms expanded; data/class kinds known; signatures canonical |
| Infer | typed AST | most-general types; constraints and evidence recorded |
| Elaborate | Core | dictionary arguments explicit; patterns compiled to cases |
| Evaluate | value / IO | call-by-need; runtime failures are structured diagnostics |

## Diagnostics

All phases render a normalized diagnostic:

```text
phase=<phase>; source=<source>; span=<span>; message=<message>
```

Phases are `parse`, `rename`, `kind`, `infer`, `desugar`, `eval`, and
`pipeline`.  Source spans are present for parse, rename, kind, and inference
errors.  Evaluation errors carry the source file and a structured message;
they do not yet carry a source span because Core does not retain spans.

## Scope and deliberate limitations

The following are **not** implemented, and are documented as such rather than
silently accepted:

- modules and imports (the book also defers module namespaces);
- the full GADT type system (GADT-style declaration syntax is parsed and
  checked as ordinary algebraic data declarations);
- polymorphic recursion, defaulting rules, and overloaded literals;
- class default methods and method-specific class constraints;
- `deriving` instance generation (the parser reports it as unsupported);
- record construction/update syntax (record declarations and selector
  functions are supported; constructors remain positional);
- negative literal patterns and layout inside parentheses;
- newtypes, type families, GADTs, and other GHC extensions;
- a Core type checker, STG/Imp transformations, LLVM, native code generation,
  and optimization.  The book's Code Generation chapter is a tutorial stub for
  the later Fun language, not a ProtoHaskell backend.

These boundaries match the book's ProtoHaskell scope and the project's
low-dependency, interpreter-first constraints.

## Documentation

- `docs/language/protohaskell.md` — language reference and feature matrix
- `docs/audit/2026-11-01-audit.md` — audit findings and remediation
- `docs/plans/` — roadmap, architecture, milestones, regression matrix,
  test specification, and diagnostics policy
- `docs/process/` — declaration semantics, annotations, harness contract, and
  historical planning artifacts
- `docs/WYAH.pdf` — local copy of the reference text

## References

- Stephen Diehl, **_Write You a Haskell_** — local copy at `docs/WYAH.pdf`;
  online mirror: <https://smunix.github.io/dev.stephendiehl.com/fun/WYAH.pdf>
- Robin Milner, “A Theory of Type Polymorphism in Programming” (1978)
- Luis Damas and Robin Milner, “Principal type-schemes for functional
  programs” (1982)
- Benjamin C. Pierce, **_Types and Programming Languages_**
- Simon Peyton Jones and David Lester, **_Implementing Functional Languages:
  A Tutorial_**
- GHC Commentary: <https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler>
