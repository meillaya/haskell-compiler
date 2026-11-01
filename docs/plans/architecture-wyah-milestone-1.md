# Architecture: Interpreted ProtoHaskell

The implementation follows the book's four representations:

| Representation | Module | Form |
| --- | --- | --- |
| Source | `WYAH.Lexer`, `WYAH.Parser` | token stream + layout braces; surface AST |
| Frontend | `WYAH.Renamed`, `WYAH.Renamer`, `WYAH.Desugar` | unique binders, grouped equations, no `do`/comprehension sugar |
| Canonical typed frontend | `WYAH.Kind`, `WYAH.Type`, `WYAH.Typed`, `WYAH.Infer` | kinds, canonical types/schemes, inferred types, evidence ids |
| Core / evaluation | `WYAH.Core`, `WYAH.Elaborate`, `WYAH.Eval` | dictionary-explicit Core, call-by-need values |

## Pipeline

```text
Text
  -> tokenize + layout                 (Lexer)
  -> parse                             (Parser)
  -> rename                            (Renamer)
  -> desugar                           (Desugar)
  -> kind check + canonicalize         (Kind)
  -> infer                             (Infer)
  -> elaborate + pattern compile       (Elaborate)
  -> Core                              (Core)
  -> evaluate / run IO                 (Eval)
```

The driver in `WYAH.Repl` composes these passes and normalizes failures to the
diagnostic contract.

## Module boundaries

| Module | Responsibility |
| --- | --- |
| `WYAH.Diagnostic` | `Phase`, normalized `Diagnostic`, `CompilerError` |
| `WYAH.Span` | positions, spans, rendering |
| `WYAH.Syntax` | surface AST, literals, patterns, types, declarations, fixity |
| `WYAH.Lexer` | tokens, strings/chars, comments, layout insertion |
| `WYAH.Parser` | declarations, expressions, patterns, types, fixity resolution |
| `WYAH.Renamed` | resolved names and renamed AST |
| `WYAH.Renamer` | scopes, grouping, record selectors, recursive visibility |
| `WYAH.Type` | kinds, `TVar`, `TyCon`, `Type`, `Pred`, `Scheme`, substitution, pretty-printing |
| `WYAH.Prelude` | built-in names and type schemes |
| `WYAH.Kind` | synonym expansion, kind inference, data/class/instance environments |
| `WYAH.Typed` | inferred expression tree with evidence ids |
| `WYAH.Infer` | HM inference, constraints, generalization, instance solving |
| `WYAH.Core` | small Core IR with constructors, cases, dictionaries |
| `WYAH.Desugar` | `do` and list-comprehension desugaring |
| `WYAH.Elaborate` | evidence-to-dictionary elaboration and pattern-match compilation |
| `WYAH.Eval` | thunks, values, primitives, IO, rendering |
| `WYAH.Repl` | compile pipeline, REPL, file runner |

## Notation and invariants

### Renaming

- Each binder has a `ResolvedName` with a module-unique integer.
- Shadowing is resolved by environment lookup; the renamed AST has no
  accidental capture.
- Top-level declarations are mutually recursive; data constructors, class
  methods, and record selectors are in scope throughout the module.
- Multiple equations are grouped before inference.

### Kinds

- `Kind = KStar | KArrow Kind Kind | KPrim | KMeta Int` internally.
- Data parameters receive kind metavariables, refined by constructor field
  uses; unresolved metas default to `KStar`.
- `data T1 f a = T1 (f a)` yields `T1 :: (* -> *) -> * -> *`.
- Type synonyms are expanded before conversion and must be fully applied.
- Classes are single-parameter; method schemes contain the class predicate.

### Inference

- A mutable global substitution with occurs checks.
- Schemes are `Forall [TVar] [Pred] Type`.
- Each variable occurrence records evidence ids, one per instantiated
  predicate.
- Generalization quantifies variables not free in the environment and turns
  constraints over those variables into scheme predicates.
- Signature checking skolemizes quantified variables and treats signature
  predicates as givens.
- Instances are matched one-way against predicate types; instance contexts
  are solved recursively with a depth bound.

### Elaboration

- Each top-level binding with predicates takes dictionary parameters.
- Class methods become selector functions (`dict -> method`).
- Instances become dictionary values; superclass fields are built by solving
  the superclass predicates.
- Function equations compile to argument lambdas and nested Core cases with a
  failure continuation that tries the next equation.
- `where` blocks become `CLetRec`.
- `if` becomes a case on the boolean value; guards become nested cases.
- `do` and comprehensions are already gone before Core.

### Evaluation

- `Thunk = Thunk Env CoreExpr | ThunkValue Value | ThunkDelay (Either EvalError Value)`.
- Call-by-need: application passes a thunk; constructor fields are thunks.
- `CLetRec` ties the recursive environment knot in Haskell.
- Dictionaries are runtime values and method selection is explicit.
- `IO` is a value containing an `IO (Either EvalError Value)` action; the
  driver runs it.

## Extension seams

- A Core type checker can be added between `Elaborate` and `Eval`.
- Records construction/update and `deriving` can be desugared in `Renamer` or
  `Desugar` without changing inference.
- Function-specific class constraints would extend `ClassDef`/elaboration.
- STG/Imp/LLVM belong to the book's later Fun language, not this ProtoHaskell
  pipeline.
