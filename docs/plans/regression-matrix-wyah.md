# Regression Matrix: WYAH

| ID | Source Chapter | Example / Behavior | Expected Semantics | Owning Stage | Status |
| --- | --- | --- | --- | --- | --- |
| P1 | Parsing | integer literal parses | AST node created with span | Parser | active-now |
| P2 | Parsing | lambda parses | lambda AST shape preserved | Parser | active-now |
| P3 | Parsing | malformed token stream fails | parse error with normalized phase/source/span diagnostics | Parser | active-now |
| P4 | Parsing | `let` expression parses | `let` syntax lowers into expression semantics | Parser | active-now |
| P5 | Parsing | `if` expression parses | conditional surface syntax preserved | Parser | active-now |
| P6 | Parsing | boolean literal parses | `true`/`false` become boolean literals | Parser | active-now |
| P7 | Parsing | `+` operator parses | surface-only operator syntax desugars to existing builtins | Parser | active-now |
| P8 | Parsing | prefix `-` parses | unary minus surface form desugars to existing builtin semantics | Parser | active-now |
| PR1 | Program Parsing | single top-level declaration parses | `Program [Decl] Expr` shape is preserved | Parser | active-now |
| PR2 | Program Parsing | missing semicolon rejected | declaration separator errors are reported structurally | Parser | active-now |
| PR3 | Program Parsing | missing final expression rejected | program shape requires final expression | Parser | active-now |
| N1 | Renamer | unbound variable `y` in `\x -> y` | reject with normalized source-aware diagnostic | Name | active-now |
| N2 | Renamer | duplicate top-level declaration | reject duplicate top-level names | Name | active-now |
| N3 | Renamer | forward reference at top level | declarations are sequential and non-recursive | Name | active-now |
| T1 | Type Systems / HM | `id` infers `forall a. a -> a` | principal type reported | Infer | active-now |
| T2 | HM | `const` infers `forall a b. a -> b -> a` | principal type reported | Infer | active-now |
| T3 | HM | `\x -> x x` fails occurs check | infinite type rejected | Infer | active-now |
| T4 | HM | boolean literal infers `Bool` | boolean type reported | Infer | active-now |
| T5 | HM | `if true then 1 else 0` infers `Int` | branches unify | Infer | active-now |
| T6 | HM | branch mismatch rejected | conditional branches must have same type | Infer | active-now |
| T7 | HM | non-boolean guard rejected | condition must be `Bool` | Infer | active-now |
| T8 | HM | builtin `succ` infers `Int -> Int` | builtin environment exposes primitive type | Infer | active-now |
| T9 | HM | builtin `add` infers `Int -> Int -> Int` | builtin environment exposes primitive type | Infer | active-now |
| T10 | HM | `let`-polymorphism reuses identity | generalization/instantiation works for current interpreter scope | Infer | active-now |
| T11 | HM | operator expressions infer through desugaring | `1 + succ 2` preserves existing typing semantics | Infer | active-now |
| TP1 | Program Types | program final expression infers after declarations | top-level declarations extend later typing environments | Infer | active-now |
| E1 | Evaluation | application evaluates | closures apply through environments | Eval | active-now |
| E2 | Evaluation | `let` evaluates | bound expression extends scope | Eval | active-now |
| E3 | Evaluation | `if` true/false branches evaluate | guard controls branch selection | Eval | active-now |
| E4 | Evaluation | `succ`, `pred`, `iszero`, `add` evaluate | builtins execute through predefined runtime environment | Eval | active-now |
| E5 | Evaluation | operator expressions evaluate | operator surface desugars to existing builtin semantics | Eval | active-now |
| EP1 | Program Evaluation | sequential declarations evaluate | program lowers after validation/renaming to ordered lets | Eval | active-now |
| EP2 | Pipeline | ill-typed application rejected before eval | type failure prevents invalid runtime execution | Infer | active-now |
| F1 | Datatypes | minimal datatype constructor behavior | compatibility marker only | Core | staged-for-later |
| F2 | Extended Parser | richer operator/layout case | compatibility marker only | Parser | staged-for-later |
| F3 | Program Semantics | recursive declaration groups | deferred until a later program-semantics target | Name/Infer/Eval | staged-for-later |
| B1 | LLVM | simple arithmetic codegen | blocked by Milestone 1 non-goal | Core/Backend | blocked-by-non-goal |
| B2 | Typeclasses | overloaded plus via dictionary passing | blocked by Milestone 1 non-goal | Infer/Core | blocked-by-non-goal |

## Notes
- `active-now` tests should inform and protect the current typed-interpreter baseline.
- `staged-for-later` tests shape the architecture but do not gate Milestone 1 completion yet.
- `blocked-by-non-goal` tests document future direction and must not widen current scope.
