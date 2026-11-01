# Diagnostics Assertion Policy

Diagnostics are part of the compiler contract, but assertions should be stable
under reasonable wording changes.

## Principle

Assertions are structural by default and textual only when necessary.

## Structural assertions

Tests assert:

- the phase that failed (`parse`, `rename`, `kind`, `infer`, `eval`,
  `pipeline`);
- the presence and value of a source span where applicable;
- the semantic failure category (`occurs check`, `cannot unify`,
  `not in scope`, `missing instance`, `pattern match failure`);
- the essential payload (type names, class name, variable name).

## Textual assertions

Short, deliberately stable fragments are appropriate:

- `phase=infer`
- `cannot unify`
- `occurs check failed`
- `not in scope`
- `division by zero`
- `pattern match failure`

Full formatted diagnostics are still available to users; tests avoid
locking in prose where a fragment is sufficient.

## Current phase guarantees

- parse/rename/kind/infer errors carry a source span;
- evaluation errors carry the source file but not a span (Core is span-free);
- pipeline errors are normalized wrappers.

## Fixture guidance

- prefer `infer-type`/`eval-value`/`program-value` for success cases;
- use `infer-error`/`rename-error`/`kind-error`/`eval-error` with a short
  fragment;
- assert exact spans only when span behavior is the subject of the test.
