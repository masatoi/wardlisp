# String type — design & implementation plan

> Branch: `feat/string-type` (base: `main`, v0.2.0 → 0.3.0)
> Driver: recurya novel-engine. Requirements live in recurya
> `docs/plans/2026-06-27-wardlisp-extension-requirements.md` (R1–R5).

## Problem

wardlisp has no string type. Symbols are bare lowercase CL strings, so a string
value represented as a CL string would be indistinguishable from a symbol. The
host (recurya) needs to evaluate scene expressions and walk the returned data,
telling apart *symbols* (directive tags like `say`), *strings* (dialogue text)
and *numbers*.

## Core decision: box strings, keep symbols bare

- **Symbol** = bare lowercase CL `string` (unchanged).
- **String** = new boxed struct `wstring` holding a CL `string`.

`wstring-p` is false for symbols and vice-versa, so the two never collide.

## Changes by file

- `src/types.lisp`
  - `defstruct wstring` (constructor `make-wstring`, accessor `wstring-value`).
  - Export `wstring make-wstring wstring-p wstring-value`.
- `src/reader.lisp`
  - `+max-string-length+` (literal length cap, parse-time guard).
  - `read-string`: reads `"..."` with escapes `\" \\ \n \t`; returns a
    `wstring` AST node. `atom-char-p` already excludes `"`.
  - dispatch `#\"` in `read-expr`.
- `src/evaluator.lisp`
  - `eval-inner`: `wstring` self-evaluates; charges `track-cons` ∝ length.
  - `ast-to-value` (quote): pass `wstring` through, charging ∝ length.
- `src/builtins.lisp`
  - `print-value`: print `wstring` as `"..."` with escapes; symbols stay bare.
  - `wardlisp-equal` / `builtin-eq-p`: content-compare strings.
  - new builtins `string-append`, `number->string`, `string-length`
    (registered in `make-initial-env`). String creation charges memory ∝ length.
- `src/main.lisp` (`:wardlisp` public API)
  - Export list-walk: `ocons-p ocons-ocar ocons-ocdr`.
  - Export string API: `string-value-p string-value make-string-value`.
  - Export type predicates: `symbol-value-p number-value-p`.
  - `evaluate` gains optional `:bindings` alist `(("name" . value) …)` injected
    into the initial environment (R4).

## Memory model (R1.6 / R5)

String creation (literal eval, quote, `string-append`, `number->string`) charges
`track-cons` by `(max 1 length)`, so large/looped strings hit `max-cons`. Literal
length is additionally capped at parse time by `+max-string-length+`. fuel /
depth / output / timeout / integer bounds are untouched.

## Out of scope

`substring`, char ops, mutation, `string<->symbol`, regex. Directive vocabulary
and the player live entirely in the host.
