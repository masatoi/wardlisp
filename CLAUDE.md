# WardLisp

Restricted Lisp dialect for educational games — sandboxed evaluator with resource limits.

## Agent Guidelines

@prompts/repl-driven-development.md
@prompts/common-lisp-expert.md

## Quick Reference

```lisp
;; Run tests (via cl-mcp)
run-tests(system: "wardlisp/tests")

;; Single test
run-tests(system: "wardlisp/tests", test: "wardlisp/tests/evaluator-test::test-name")

;; Lint (before commit)
mallet src/*.lisp
```

## Tech Stack

- **Language**: Common Lisp (SBCL)
- **Build**: ASDF — single `wardlisp` system (not package-inferred)
- **Testing**: Rove

## Self-Hosted Development

All Lisp operations go through cl-mcp tools as defined in `repl-driven-development.md`. This applies to **Claude Code's built-in tools as well** — do not use Read, Edit, Write, Grep, or Glob for `.lisp` / `.asd` files. Use `clgrep-search`, `lisp-read-file`, `lisp-edit-form`, `lisp-patch-form`, `repl-eval`, `load-system`, `run-tests` instead.

**Allowed shell commands**: `git`, `gh`, `mallet`, and user-requested commands only.

**First-time setup**: Call `fs-set-project-root` with `{"path": "."}` before file operations.

## Common Mistakes

- **Forgetting `load-system`**: `lisp-edit-form` writes to disk only — the worker needs `load-system` to see changes.
- **`lisp-patch-form` whitespace**: `old_text` must match exactly. Use `lisp-read-file collapsed=false` to see exact text first.
- **Test fallback**: When package conflicts occur, use `rove wardlisp.asd` from Bash for a clean process.

## Architecture

```
src/
  types.lisp      — Data types: closure, tail-call, ocons, builtin, exec-ctx, conditions
  reader.lisp     — S-expression parser (sandboxed: no #, no :)
  env.lisp        — Lexical environment (extend, lookup, set!)
  builtins.lisp   — Built-in functions (+, -, *, /, cons, car, etc.)
  evaluator.lisp  — Core evaluator with TCO trampoline, local define (letrec*)
  main.lisp       — Public API: evaluate function with metrics

tests/             Rove test suites (mirrored naming: *-test.lisp)
docs/              Language specification
prompts/           System prompts for AI agents
```

### Key Design Decisions

- **Sandbox**: String-based symbols, custom `ocons` type, reader blocks `#` and `:`
- **TCO**: Trampoline via `tail-call` structs with `:expr`/`:body` kinds
- **Local define**: Scheme-style letrec* in body head position (lambda, define, let, let*)
- **Resource limits**: fuel, max-depth, max-cons, max-output, max-integer, timeout
- **Builtin protection**: First env frame is builtins, `top-level-update-or-append` skips it

## Code Style

- Google Common Lisp Style Guide
- 2-space indent, <=100 columns, blank line between top-level forms
- Lower-case lisp-case: `my-function`, `*special*`, `+constant+`, `something-p`
- Docstrings required for public functions
- Each file starts with `(in-package ...)`
