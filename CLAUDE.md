# WardLisp

Restricted Lisp dialect for educational games — sandboxed evaluator with resource limits.

## Agent Guidelines

@prompts/repl-driven-development.md
@prompts/common-lisp-expert.md
@prompts/cl-spec-driven-development.md

## Quick Reference

```lisp
;; Run tests (via cl-mcp; run-tests reloads the test system itself)
run-tests(system: "wardlisp/tests")

;; Single test
run-tests(system: "wardlisp/tests", test: "wardlisp/tests/evaluator-test::test-name")

;; Contracts (cl-spec tool group): load the backend, then the bundle
load-system(system: "cl-spec/check-it")
load-system(system: "wardlisp/specs")
spec-symbol(symbol: "wardlisp:evaluate")
spec-check(function: "wardlisp:evaluate", trials: 200)
spec-check(symbol: "wardlisp:evaluate")

;; Lint (before commit; the same globs CI would run)
mallet src/*.lisp specs/*.lisp scripts/*.lisp
```

## Tech Stack

- **Language**: Common Lisp (SBCL)
- **Build**: ASDF — `wardlisp` (not package-inferred), plus `wardlisp/tests` and the opt-in
  `wardlisp/specs`, all in `wardlisp.asd`
- **Testing**: Rove; executable contracts with cl-spec (`docs/specs.md`)

## Self-Hosted Development

All Lisp operations go through cl-mcp tools as defined in `repl-driven-development.md`. This applies to **Claude Code's built-in tools as well** — do not use Read, Edit, Write, Grep, or Glob for `.lisp` / `.asd` files. Use `clgrep-search`, `lisp-read-file`, `lisp-edit-form`, `lisp-patch-form`, `repl-eval`, `load-system`, `run-tests` and the `spec-*` tools instead.

**Allowed shell commands**: `git`, `gh`, `mallet`, `rove` (test fallback), `ros run --load scripts/check-specs.lisp` (contract fallback, as CI runs it), and user-requested commands only.

**First-time setup**: Call `fs-set-project-root` with the absolute path of the repository root before file operations (a relative path resolves on the server's side). The root lasts for the session; set it again after a reconnect.

**cl-spec tools**: `spec-list`, `spec-symbol`, `spec-describe` and `spec-check` exist only when the cl-mcp server was started with the `cl-spec` tool group (`MCP_ENABLE_TOOL_GROUPS=cl-spec`). If they are missing, ask the user to restart the server with it; if a call says `cl-spec-not-loaded`, `load-system` `cl-spec/check-it` and `wardlisp/specs`.

## Contracts

`wardlisp/specs` (`specs/`, see `docs/specs.md`) holds cl-spec Function Specs on `evaluate`, `print-value`, `make-string-value`, `string-value`, `string-value-p`, `symbol-value-p` and `number-value-p`, and Properties derived from `docs/language-spec.md` (resource limits, arithmetic, print/read round trip, `equal?`, host bindings and strings). CI's `specs` job runs every one under fixed seeds, plus a negative control that requires each known-wrong implementation to be caught.

When a change touches the evaluator, the reader, a builtin, `print-value` or the host API:

1. `load-system` `cl-spec/check-it`, then `wardlisp/specs`.
2. Read the contracts with `spec-symbol` / `spec-describe` before editing, and take a baseline with `spec-check function=` **and** `spec-check symbol=` (they run different things).
3. Edit, then `load-system` `wardlisp` with `clear_fasls: true` and `wardlisp/specs` again, and re-check with the seed and digest from the `Replay:` line, then without a seed, then `run-tests`.

Never weaken a contract to make a check pass. A contract that disagrees with a requested behaviour is brought to the user first; a behaviour change the user asks for updates `docs/language-spec.md` and the contract together. A new Function Spec or Property must also be added to `contract-names` / `property-names` in `specs/runner.lisp`, or the runner fails the bundle.

## Common Mistakes

- **Forgetting `load-system`**: `lisp-edit-form` writes to disk only — `repl-eval`, `code-*` and `spec-*` see changes only after `load-system` (`run-tests` reloads by itself).
- **Stale contracts after an edit**: reload `wardlisp` with `clear_fasls: true`, then `wardlisp/specs`; reloading only one leaves the other stale.
- **`lisp-patch-form` whitespace**: `old_text` must match exactly. Use `lisp-read-file collapsed=false` to see exact text first.
- **`RESULT` in a contract**: `:post` finds `RESULT` through the parameters' package. A parameter named by a CL symbol (`string`, `list`, `count`) breaks that; name it something else (`text`).
- **Negative control in the MCP worker**: `run-negative-control` replaces production functions while it runs; prefer `WARDLISP_SPECS_MODE=negative-control ros run --load scripts/check-specs.lisp`.
- **Test fallback**: When package conflicts occur, use `rove wardlisp.asd` from Bash for a clean process.

## Architecture

```
src/
  types.lisp      — Data types: closure, tail-call, ocons, wstring, builtin, exec-ctx, conditions
  reader.lisp     — S-expression parser (sandboxed: no #, no :)
  env.lisp        — Lexical environment (extend, lookup, set!)
  builtins.lisp   — Built-in functions (+, -, *, /, cons, car, etc.) and print-value
  evaluator.lisp  — Core evaluator with TCO trampoline, local define (letrec*)
  main.lisp       — Public API: evaluate with metrics, value introspection

tests/             Rove test suites (mirrored naming: *-test.lisp)
specs/             cl-spec contracts on the public API and their runner (wardlisp/specs)
scripts/           check-specs.lisp: command-line entry for the contracts (CI)
docs/              Language specification (language-spec.md), contracts (specs.md)
prompts/           System prompts for AI agents
.github/workflows/ CI: `test` (rove) and `specs` (cl-spec, pinned revision)
```

### Key Design Decisions

- **Sandbox**: String-based symbols, custom `ocons` type, reader blocks `#` and `:`
- **TCO**: Trampoline via `tail-call` structs with `:expr`/`:body` kinds
- **Local define**: Scheme-style letrec* in body head position (lambda, define, let, let*)
- **Resource limits**: fuel, max-depth, max-cons, max-output, max-integer, timeout
- **Builtin protection**: First env frame is builtins, `top-level-update-or-append` skips it
- **Contracts are opt-in**: `wardlisp` never depends on cl-spec; only `wardlisp/specs` does

## Code Style

- Google Common Lisp Style Guide
- 2-space indent, <=100 columns, blank line between top-level forms
- Lower-case lisp-case: `my-function`, `*special*`, `+constant+`, `something-p`
- Docstrings required for public functions
- Each file starts with `(in-package ...)`
