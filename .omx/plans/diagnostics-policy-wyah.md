# Diagnostics Policy: WYAH

## Minimal Span Model
- Each parseable term should carry at least line/column or equivalent source span metadata where practical.
- Errors should identify the compiler stage that raised them.

## Parser Error Policy
- Invalid syntax must report file/input context and location.

## Renamer Error Policy
- Unbound names and duplicate bindings should reference relevant source locations.

## Type Inference Error Policy
- Type mismatch and occurs-check failures should point to the originating expression span where practical.

## REPL Error Policy
- Errors must be concise, stage-labeled, and learner-readable.

## Non-goals for Diagnostics Polish
- No requirement yet for rich compiler-grade pretty diagnostics.
- No requirement yet for full GHC-style multi-span notes.
- The goal is reliable, useful debugging information early, not polished presentation.
