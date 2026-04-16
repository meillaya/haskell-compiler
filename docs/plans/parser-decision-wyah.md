# Parser Decision Record: WYAH

## Chosen Strategy
Default to a **minimal-dependency parser path** for Milestone 1.

## Alternatives Considered
### Handwritten / minimal parser (chosen)
- Preserves understanding of parsing mechanics.
- Keeps dependencies low.
- Best fit for the learning goal.

### Lightweight parser library (rejected for now)
- Could improve ergonomics and diagnostics.
- Rejected initially because it adds abstraction/dependency overhead before we prove the minimal approach is insufficient.

## Dependency Rationale
Use no external parser dependency unless there is concrete evidence that source-span quality, grammar maintenance, or correctness would otherwise be materially worse.

## Source-Span Implications
Any parser approach must preserve file/input context and line/column or equivalent spans on parse failures and AST nodes where practical.

## Revisit Trigger
Revisit this decision if:
- maintaining the grammar becomes materially harder than expected,
- source-span fidelity cannot be preserved cleanly,
- or Milestone 2 parser requirements clearly exceed the minimal approach.
