# Test Specification: WYAH Compiler Learning Roadmap

## Test categories

- parser acceptance, structural spans, layout and fixity;
- renamer/scope resolution and duplicate checks;
- synonym expansion and kind inference;
- Hindley–Milner inference, generalization, annotations, signatures;
- dictionary/instance resolution and superclasses;
- evaluator semantics, laziness, IO, and runtime failures;
- forward-looking/staged markers for unsupported features.

## Chapter mapping

| Area | Primary pass | Fixture group | Status |
| --- | --- | --- | --- |
| Parsing / layout | Parser | parser.tsv, inline | active |
| Lambda calculus / evaluation | Eval | eval.tsv, inline | active |
| Type systems / HM | Infer | infer.tsv, inline | active |
| ProtoHaskell frontend | Renamer/Kind/Infer | program.tsv, inline | active |
| Renamer | Renamer | rename.tsv, inline | active |
| Datatypes/records | Kind/Infer/Eval | program.tsv, eval.tsv | active |
| Typeclasses | Kind/Infer/Elaborate/Eval | classes.tsv, infer.tsv | active |
| Extended parser | Parser | parser.tsv | active (handwritten) |
| LLVM/codegen | Backend | none | out-of-book |

## Negative tests

- unbound variable;
- duplicate datatype/class/method/record field;
- non-linear pattern;
- malformed syntax and end-of-input;
- kind mismatch;
- unification failure, occurs check, non-boolean guard;
- missing instance;
- signature/annotation mismatch;
- pattern-match failure, division by zero, non-exhaustive function.

## Status policy

- `active-now`: part of the current contract and enforced.
- `staged-for-later`: documented, skipped by the runner.
- `blocked-by-non-goal`: intentionally out of scope.

Skipped rows remain visible in fixtures but do not count as passing tests.

## Coverage minimums

The suite exceeds the original minimums:

- parser: literals, spans, layout, fixity, sections, ranges, records, GADTs;
- renamer: unbound, duplicates, shadowing, recursion, selectors;
- kinds: higher-kinded parameters, kind errors, synonyms;
- inference: id/const/compose, let-polymorphism, annotations, signatures,
  class constraints, occurs checks;
- evaluator: arithmetic, ADTs, guards/where, lists/tuples/comprehensions,
  records, classes, laziness, IO, runtime errors.

## Verification commands

```bash
cabal build all
cabal test
stack test        # when Stack is available
devenv test       # when devenv is available
```

## Evidence expectations

- Tests run all groups and report a summary.
- Positive cases assert types or rendered values.
- Negative cases assert phase plus a stable semantic fragment.
- Structural parser tests assert spans/nesting independently of fixtures.
