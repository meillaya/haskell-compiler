# Parser Decision Record: WYAH

## Decision

Use a **handwritten lexer + layout algorithm + recursive-descent parser** with
a fixity table and a small backtracking combinator.  No Alex/Happy/Parsec
dependency is required.

## Why

- The book's learning goal is to understand parsing mechanics, not to hide
  them behind a parser generator.
- Layout is the subtle part; an explicit layout pass makes virtual braces and
  semicolons inspectable.
- The grammar is small enough that precedence climbing, pattern parsing, and
  declaration parsing remain readable.
- The project keeps a base-only dependency surface.

## Consequences

- Fixity declarations are collected in a prepass so operator use can precede
  the declaration.
- Layout insertion is suspended inside parentheses/brackets; explicit
  `{}`/`;` is required for multi-line blocks there.
- The parser is tested structurally (spans, nesting, layout blocks) as well as
  through fixtures.
- Alex/Happy remain documented future options for the book's Extended Parser
  chapter if the grammar grows beyond this implementation.

## Revisit triggers

- Layout inside parentheses becomes common enough to justify a fuller
  offside-rule implementation.
- The grammar grows to need generated lexer/parser tables.
- Source-span fidelity or error recovery becomes materially harder.
