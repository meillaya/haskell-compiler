# Declaration Semantics

This document defines the current program-level contract for top-level declarations.

## Program representation

A program is represented as:

- an ordered list of top-level declarations, and
- a required final expression.

In Haskell terms, the current surface contract is roughly:

```haskell
data Program = Program [Decl] Expr
```

where each declaration binds exactly one name to one expression.

## Ordering and visibility invariants

Top-level declarations are currently **sequential**.

That means:

- earlier declarations are visible to later declarations,
- earlier declarations are visible to the final expression,
- later declarations are **not** visible to earlier declarations,
- forward references are rejected,
- duplicate top-level names are rejected.

## Recursion policy

Top-level declarations are currently **non-recursive**.

The project does **not** yet support:

- self-recursive declarations,
- mutually recursive declaration groups,
- module-level dependency analysis.

This is intentional. The current milestone is still a semantics-first typed interpreter, and non-recursive declarations keep the program model bounded and easier to reason about.

## Final expression rule

A program must end in a final expression.

For example, this is valid:

```haskell
id = \x -> x;
id 42
```

and this is invalid because there is no final expression:

```haskell
id = \x -> x;
```

## Lowering boundary

The parser and renamer operate on first-class `Program` / `Decl` structures.

Only **after** program-shape validation and name resolution does the implementation lower a program into expression-level structure for the existing typed interpreter pipeline.

At the moment, lowering is equivalent to nested `let` construction over the ordered declaration list.

Conceptually:

```haskell
x = e1;
y = e2;
body
```

lowers to something like:

```haskell
let x = e1 in
let y = e2 in
body
```

The important invariant is that lowering happens **after** the program contract has been enforced, not as a parser shortcut.

## Minimal diagnostic spine for program errors

Current program-level errors should preserve at least:

- phase attribution,
- source span presence when source-derived,
- stable error kind.

Important current program/declaration error classes:

- duplicate top-level declaration,
- forward-reference / visibility error,
- malformed program shape,
- missing final expression,
- malformed declaration separator usage.
