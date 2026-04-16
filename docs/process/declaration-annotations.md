# Declaration Annotations

This document defines the current top-level declaration annotation slice.

## Status

Top-level declaration annotations are **optional**.

Unannotated declarations continue to infer exactly as before. Annotated declarations add a user-written type claim that is checked against the inferred type of the declaration.

## Surface form

The current annotation surface is:

```haskell
id : Int -> Int;
id = \x -> x;
id 42
```

An annotation must be followed immediately by a declaration of the same name.

## Checking rule

For an annotated declaration:

1. parse the annotation into the current surface type language,
2. rename the declaration as usual,
3. infer the declaration body,
4. check that the inferred type unifies with the declared type,
5. if it succeeds, store the declared type in the program environment for later uses.

## Current type syntax in annotations

The current annotation language intentionally remains small:

- `Int`
- `Bool`
- function arrows `->`
- parenthesized function types

Examples:

```haskell
succLike : Int -> Int;
succLike = \x -> succ x;
succLike 0
```

```haskell
chooser : Bool -> Int -> Int -> Int;
chooser = \b -> \x -> \y -> if b then x else y;
chooser true 1 2
```

## Failure modes

Current expected annotation failures include:

- malformed annotation syntax,
- unsupported annotation type syntax,
- annotated declaration missing its matching definition,
- annotation mismatch between declared and inferred type.

Diagnostics for these failures should continue to use the normalized phase/source/span/message rendering contract.

## Unchanged invariants

This slice does **not** change the current program model invariants:

- final expression remains mandatory,
- top-level declarations remain sequential,
- top-level declarations remain non-recursive,
- duplicate names are still rejected,
- no broader grammar-family work is included in the same slice.
