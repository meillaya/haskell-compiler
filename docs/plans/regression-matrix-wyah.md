# Regression Matrix: WYAH

Status legend: `active` (enforced by the test suite), `staged` (documented
future work), `out-of-book` (not part of ProtoHaskell).

## Parsing

| ID | Behaviour | Test source | Status |
| --- | --- | --- | --- |
| P1 | Integer/char/string literals | `parser.tsv`, inline | active |
| P2 | Lambda spans | `parser.tsv`, inline | active |
| P3 | `let` expression | `parser.tsv`, inline | active |
| P4 | `if`/`case` expressions | `parser.tsv`, `layout-case` | active |
| P5 | Operator precedence/fixity | `parse-add`, `parse-precedence` | active |
| P6 | Prefix negation | `parse-negative`, `eval-negate` | active |
| P7 | Lists, tuples, ranges | `parse-tuple-section`, `parse-list-range` | active |
| P8 | Sections | `parse-op-section` | active |
| P9 | Comments | inline `commentModule` | active |
| P10 | Layout `where`/`do`/`case`/`class` | `layout-*` fixtures | active |
| P11 | Data/record/GADT syntax | `parse-record-data`, `parse-gadt-data` | active |
| P12 | Parse errors with spans | `parse-error-*` | active |
| P13 | Layout inside parentheses | none | staged (explicit braces required) |

## Renaming and scope

| ID | Behaviour | Test source | Status |
| --- | --- | --- | --- |
| N1 | Unbound variable rejected | `rename-unbound` | active |
| N2 | Duplicate datatype rejected | `rename-dup-data` | active |
| N3 | Non-linear pattern rejected | `rename-dup-pattern` | active |
| N4 | Shadowing keeps inner binder | `shadowModule` | active |
| N5 | Recursive top-level visibility | `rename-forward`, `program-mutual` | active |
| N6 | Record selector generation | `eval-record` | active |
| N7 | Class method resolution | `classes.tsv` | active |

## Kinds

| ID | Behaviour | Test source | Status |
| --- | --- | --- | --- |
| K1 | Higher-kinded data parameter | inline `higherKindedModule` | active |
| K2 | Ill-kinded constructor rejected | inline `illKindedModule` | active |
| K3 | Type synonym expansion/saturation | `infer-type-synonym` | active |
| K4 | Class/superclass kind checking | `classes.tsv` | active |

## Inference

| ID | Behaviour | Test source | Status |
| --- | --- | --- | --- |
| T1 | Identity/const/composition | `infer-id`, `infer-const`, `infer-compose` | active |
| T2 | Let-polymorphism | `infer-let-poly` | active |
| T3 | Occurs check | `infer-occurs` | active |
| T4 | Branch/guard type errors | `infer-branch-mismatch`, `infer-non-bool` | active |
| T5 | Explicit signature checking | `infer-annotated`, `infer-signature-mismatch` | active |
| T6 | Expression annotations | `infer-annotation*` | active |
| T7 | Higher-kinded constructor use | `infer-higher-kinded` | active |
| T8 | Class scheme predicates | `infer-class-scheme` | active |
| T9 | Missing instance rejected | `class-context-fail` | active |
| T10 | Polymorphic instance contexts | `eval-instance-context` | active |

## Evaluation

| ID | Behaviour | Test source | Status |
| --- | --- | --- | --- |
| E1 | Arithmetic/precedence/negation | `eval-arithmetic`, `eval-negate` | active |
| E2 | Application/let/if | `eval-application`, `eval-let`, `eval-if-*` | active |
| E3 | ADTs and pattern matching | `eval-case`, `eval-factorial` | active |
| E4 | Guards/where/recursion | `eval-guards`, `eval-where`, `eval-recursive-let` | active |
| E5 | Laziness/infinite lists | `eval-primes`, `eval-take-range` | active |
| E6 | Lists/tuples/comprehensions | `eval-map`, `eval-filter`, `eval-comprehension` | active |
| E7 | Records/selectors | `eval-record` | active |
| E8 | Custom operators | `eval-operator` | active |
| E9 | IO/do notation | `eval-do-io`, `program-do-let` | active |
| E10 | Pattern-match failure | `eval-pattern-fail` | active |
| E11 | `fix` | `eval-fix`, `infer-fix` | active |
| E12 | Pattern guards/as-patterns | `eval-pattern-guard`, `eval-as-pattern` | active |

## Typeclasses

| ID | Behaviour | Test source | Status |
| --- | --- | --- | --- |
| C1 | Int instance | `class-int` | active |
| C2 | Bool instance | `class-bool` | active |
| C3 | Parametric list instance | `class-list` | active |
| C4 | Superclass selection | `eval-superclass` | active |
| C5 | Instance context dictionaries | `eval-instance-context` | active |
| C6 | Default methods | none | staged |
| C7 | Method-specific constraints | none | staged |
| C8 | `deriving` generation | none | staged |

## Out-of-book boundaries

| ID | Behaviour | Status |
| --- | --- | --- |
| B1 | LLVM/STG/Imp/codegen | out-of-book |
| B2 | Modules/imports | out-of-book |
| B3 | Full GADT type system | out-of-book (syntax only) |
| B4 | Record construction/update syntax | staged |
| B5 | Newtypes/type families/GHC extensions | out-of-book |
