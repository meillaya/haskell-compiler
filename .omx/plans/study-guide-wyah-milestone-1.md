# 1-Page Study Guide: WYAH Milestone 1

## Goal
Build a faithful typed interpreter / REPL for a small WYAH-inspired language before touching LLVM.

## Core Concept Sequence
1. **Compiler pipeline**: source -> parse -> rename -> infer -> desugar -> evaluate.
2. **Lambda calculus**: variables, abstraction, application, substitution, reduction.
3. **Names and scope**: binding structure matters before types do; renaming prevents capture and resolves meaning.
4. **Type systems**: typing judgments prevent nonsense terms from reaching evaluation.
5. **Hindley–Milner**: constraints + unification + generalization/instantiation give principal types.
6. **Evaluation**: use a small typed core whose runtime behavior is easy to reason about.
7. **ProtoHaskell mindset**: preserve semantics, keep the source language teachable, and use explicit IR boundaries.

## Mental Model for the Project
- **Surface AST**: parsed syntax with spans, not yet resolved.
- **Renamed AST**: names are uniquely bound or rejected.
- **Typed/Core IR**: smaller semantics-bearing language.
- **Evaluator/REPL**: prove the semantics before backend complexity.

## Milestone 1 Success Bar
- Parses representative book examples.
- Rejects bad syntax, bad scope, and bad types intentionally.
- Infers expected HM types for core examples.
- Evaluates implemented constructs with book-faithful semantics.
- Is backed by a broad regression matrix derived from the text.

## Defer Until Later
- LLVM / native codegen
- Optimizers
- Typeclasses
- Advanced ADTs / pattern compilation
- Layout-sensitive parsing
- Full Haskell language ambition
