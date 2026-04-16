# Diagnostics Assertion Policy

Diagnostics are part of the compiler's semantic contract, but they should be tested in a way that is stable under reasonable wording changes.

## Principle

Assertions should be **structural by default** and **textual only when necessary**.

## Structural assertions

Whenever possible, tests should assert:

- the phase that failed (`parse`, `rename`, `infer`, `eval`, or `pipeline`),
- the presence of source span or location information,
- the semantic failure category,
- the essential payload of the failure.

Examples of semantic failure categories include:

- malformed syntax,
- unbound variable,
- occurs-check failure,
- branch type mismatch,
- non-boolean conditional guard,
- rejected application of a non-function.

## Textual assertions

Text matching should be limited to short canonical fragments that are intentionally stable.

Examples:

- `"occurs check"`
- `"cannot unify"`
- `"not in scope"`

Tests should avoid matching entire pretty-printed diagnostic messages unless the full wording is deliberately standardized.

## Why this matters

Overly strict full-string assertions create brittle tests that punish harmless wording changes. Overly weak assertions hide real regressions. The middle path is to assert the semantic core of the error while preserving some freedom to improve presentation.

## Current project guidance

For the current Milestone 1 interpreter:

- parser tests should confirm file/span-bearing failures;
- rename tests should confirm scope failures with location;
- inference tests should confirm type/occurs-check mismatches with meaningful fragments and spans where available;
- evaluation tests should mostly focus on values or well-defined runtime rejection cases.

## Current normalized rendering

Diagnostics currently render in a normalized form like:

```text
phase=<phase>; source=<source>; span=<span>; message=<message>
```

`source` may be `<unknown>` when a phase does not yet preserve file-level origin information. `span` may be `<unknown>` for evaluator-level errors until deeper source propagation is implemented.
