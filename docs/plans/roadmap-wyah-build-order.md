# Build-Order Roadmap: WYAH

The build order was chosen to make semantic dependencies explicit rather than
to mirror the book's presentation order exactly.

| Order | Book area | Status | Delivered |
| --- | --- | --- | --- |
| 1 | Concepts and pipeline | complete | four representations, pass invariants |
| 2 | Haskell basics / environment | complete | Cabal, Stack, devenv, license |
| 3 | Parsing | complete | lexer, layout, recursive-descent parser, fixities |
| 4 | Lambda calculus | complete | closures, substitution by environment, call-by-need thunks |
| 5 | Type systems | complete | STLC-style checks, canonical types/kinds |
| 6 | Evaluation | complete | CBV/CBN discussion, lazy interpreter, IO actions |
| 7 | Hindley–Milner | complete | constraint generation, unification, generalization |
| 8 | Design of ProtoHaskell | complete | module/decl/expr Core boundaries |
| 9 | Engineering overview | complete | driver, REPL, dump-oriented `:core` |
| 10 | Frontend | complete | data/class/instance/fixity/signature declarations |
| 11 | Renamer | complete | unique binders, recursive scopes, selectors |
| 12 | Extended parser | complete (handwritten) | layout, comments, fixity prepass, sections |
| 13 | Datatypes | complete for ProtoHaskell | ADTs, GADT syntax, records/selectors, kind inference |
| 14 | Typeclasses | complete for single-parameter classes | dictionaries, superclasses, contexts |
| 15 | LLVM | deferred | book tutorial only; no ProtoHaskell backend |
| 16 | Code generation | deferred | Ferris/Fun backend is later work |

## Dependency graph

```text
Span/Diagnostic -> Syntax -> Lexer -> Parser -> Renamed -> Renamer
  -> Desugar -> Kind/Type -> Typed -> Infer -> Core -> Elaborate -> Eval
  -> Repl -> CLI/tests
```

## Immediate vs deferred

### Implemented

Concepts, parsing, lambda calculus, type systems, evaluation, HM inference,
ProtoHaskell design/frontend/renamer, ADTs, records, typeclasses, desugaring,
Core, interpreter, REPL, regression harness.

### Deferred

LLVM, native code generation, optimizer, STG/Imp, Core type checker, default
methods, `deriving`, record construction/update syntax.
