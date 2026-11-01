# Study Guide: Interpreted ProtoHaskell

## Goal

Understand every pass from source text to a lazy Core interpreter, using the
book's ProtoHaskell language as the target.

## Core concept sequence

1. **Pipeline**: source -> parse -> rename -> desugar -> kind -> infer ->
   elaborate -> Core -> evaluate.
2. **Lambda calculus**: variables, abstraction, application, environments,
   thunks, beta reduction by substitution in the environment.
3. **Names and scope**: binding structure first; unique `ResolvedName`s
   prevent capture; recursive scopes are explicit.
4. **Kinds**: data parameters may be higher-kinded; kinds are unified and
   defaulted; synonyms are expanded first.
5. **Hindley–Milner**: constraints, unification, occurs checks, let-
   generalization, instantiation.
6. **Qualified types**: constraints become scheme predicates; instance
   resolution produces dictionaries; superclasses select fields.
7. **Desugaring**: multiple equations, guards, where, do, comprehensions,
   ranges, sections, records/selectors.
8. **Core**: small, explicit, evidence-carrying IR; pattern matching compiles
   to cases; dictionaries are values.
9. **Evaluation**: call-by-need thunks, recursive environments, lazy lists,
   IO actions.
10. **ProtoHaskell mindset**: preserve semantics, keep the source teachable,
    and use explicit IR boundaries.

## Mental model

- **Surface AST**: parsed syntax with spans, unresolved.
- **Renamed AST**: unique binders, grouped equations, no `do` sugar.
- **Canonical typed AST**: kinds/types/schemes, evidence ids.
- **Core**: explicit dictionaries, no source sugar, evaluation-ready.
- **Runtime**: thunks/values/IO actions.

## Success bar

- Parses representative book examples.
- Rejects bad syntax, scope, kinds, and types intentionally.
- Infers expected HM/qualified types.
- Evaluates with lazy semantics and dictionary dispatch.
- Backed by a broad regression suite derived from the text.

## Suggested exercises

- Add a new primitive and its scheme/evaluator case.
- Add a new instance and inspect `:core`.
- Add a record selector shared by two constructors.
- Extend the parser with a new expression form and thread it through all
  passes.
- Add a Core type checker (the book recommends one).
