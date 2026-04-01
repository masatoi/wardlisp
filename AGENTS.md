# AGENTS.md

## Overview

`wardlisp` is a restricted Lisp dialect for safe server-side evaluation in educational games. The implementation is Common Lisp and this repository is intended to be worked on with `cl-mcp`.

## Development Workflow

Use the REPL-driven workflow from [prompts/repl-driven-development.md](/home/wiz/.roswell/local-projects/wardlisp/prompts/repl-driven-development.md):

`EXPLORE -> EXPERIMENT -> PERSIST -> VERIFY`

At the start of a session, set the project root in `cl-mcp` to the repository root before file operations.

## Tool Policy

When working on this repository, prefer `cl-mcp` tools over generic shell commands.

- Lisp search: `clgrep-search`
- Lisp file reading: `lisp-read-file`
- Existing Lisp file edits: `lisp-edit-form`, `lisp-patch-form`
- Evaluation and experiments: `repl-eval`
- System loading: `load-system`
- Test execution: `run-tests`
- Syntax validation: `lisp-check-parens`

Use shell commands only for:

- `git`
- `mallet`
- `rove` as a fallback when `run-tests` is insufficient
- explicit user-requested commands

Do not use `grep`, `sed`, `cat`, `find`, or ad hoc shell pipelines for Lisp codebase manipulation when a `cl-mcp` tool can do the job.

## Project-Specific Guidance

- Source files live in `src/`: `types.lisp`, `reader.lisp`, `env.lisp`, `builtins.lisp`, `evaluator.lisp`, `main.lisp`
- Tests live in `tests/` and use Rove
- Specs and plans live in `docs/`
- Public API exports are surfaced from `main.lisp`
- Changes to evaluator semantics should be verified both by tests and by direct `cl-mcp` runtime evaluation

## Testing

Preferred:

- Run the full suite with `run-tests` on system `wardlisp/tests`
- Run targeted tests with `run-tests` using specific test names when narrowing failures

Fallback:

- `rove wardlisp.asd`

Before finishing substantial changes:

- Reload with `load-system`
- Run relevant tests
- For evaluator or reader changes, verify behavior directly with `repl-eval` or `wardlisp:evaluate`

## Editing Rules

- Use `lisp-edit-form` or `lisp-patch-form` for existing Lisp files
- Use `fs-write-file` only when creating a brand new file
- After file edits, reload the system before trusting runtime behavior in the worker
- If the worker state looks stale or inconsistent, reset it with `pool-kill-worker` and reload

## Style

- Follow the existing Common Lisp style in the repo
- Keep names lower-case and lisp-case
- Preserve docstrings on public functions
- Keep changes minimal and consistent with the surrounding file
