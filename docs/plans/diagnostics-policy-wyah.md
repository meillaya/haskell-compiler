# Diagnostics Policy: WYAH

## Normalized contract

Every compiler phase renders:

```text
phase=<phase>; source=<source>; span=<span>; message=<message>
```

Phases: `parse`, `rename`, `kind`, `infer`, `desugar`, `eval`, `pipeline`.

`source` is `<unknown>` only when a phase genuinely has no file context.
`span` is `<unknown>` for evaluation errors, because Core does not retain
spans; all other phases carry a source span when one is available.

## Per-phase expectations

| Phase | Source | Span | Notes |
| --- | --- | --- | --- |
| parse | file | yes | token/EOF spans; end-of-input is zero-width |
| rename | file | yes | binder/use locations |
| kind | file | yes | declaration/annotation locations |
| infer | file | yes | expression/pattern locations |
| desugar | file | usually | synthesized nodes may be spanless |
| eval | file | no | structured message; source file attached |
| pipeline | file | depends | driver-normalized wrapper |

## Parser errors

Malformed syntax reports file, span, and the token found.  End-of-input errors
report the zero-width position after the last token.

## Renamer errors

Unbound variables, duplicate declarations, duplicate binders, duplicate class
methods, and unsupported declaration forms report the relevant span.

## Kind errors

Unknown type constructors, synonym arity/recursion errors, kind mismatches,
and unsupported class forms report the declaration/type span.

## Type errors

Unification failures, occurs-check failures, missing instances, and ambiguous
constraints report the originating expression or signature span.

## Evaluation errors

Runtime failures report `phase=eval`, the source file, and a stable message
(for example `pattern match failure`, `division by zero`, `head of empty
list`).  They do not claim a source span.

## REPL

Errors use the same normalized rendering; declarations and expressions are
compiled through the same pipeline as files, so diagnostics remain consistent.

## Non-goals

No GHC-style multi-span notes, caret art, suggestions, or machine-readable JSON
yet.  The contract is intentionally structural and testable.
