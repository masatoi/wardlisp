# AGENTS.md

## Overview

`wardlisp` is a restricted Lisp dialect for safe server-side evaluation in educational games. The implementation is Common Lisp and this repository is intended to be worked on with `cl-mcp`, with its `cl-spec` tool group enabled.

## Development Workflow

Follow these prompts:

- [prompts/repl-driven-development.md](prompts/repl-driven-development.md): `EXPLORE -> EXPERIMENT -> PERSIST -> VERIFY`
- [prompts/common-lisp-expert.md](prompts/common-lisp-expert.md): quality and style rules
- [prompts/cl-spec-driven-development.md](prompts/cl-spec-driven-development.md): reading, keeping and checking cl-spec contracts

At the start of a session, set the project root in `cl-mcp` to the absolute path of the repository root before file operations.

## Tool Policy

When working on this repository, prefer `cl-mcp` tools over generic shell commands.

- Lisp search: `clgrep-search`
- Lisp file reading: `lisp-read-file`
- Existing Lisp file edits: `lisp-edit-form`, `lisp-patch-form`
- Evaluation and experiments: `repl-eval`
- System loading: `load-system`
- Test execution: `run-tests`
- Contracts: `spec-list`, `spec-symbol`, `spec-describe`, `spec-check`
- Syntax validation: `lisp-check-parens`

Use shell commands only for:

- `git`, `gh`
- `mallet`
- `rove` as a fallback when `run-tests` is insufficient
- `ros run --load scripts/check-specs.lisp` as a fallback for the contracts (what CI runs)
- explicit user-requested commands

Do not use `grep`, `sed`, `cat`, `find`, or ad hoc shell pipelines for Lisp codebase manipulation when a `cl-mcp` tool can do the job.

## Project-Specific Guidance

- Source files live in `src/`: `types.lisp`, `reader.lisp`, `env.lisp`, `builtins.lisp`, `evaluator.lisp`, `main.lisp`
- Tests live in `tests/` and use Rove
- Executable contracts live in `specs/` (system `wardlisp/specs`, described in `docs/specs.md`)
- The language specification is `docs/language-spec.md`; plans live in `docs/plans/`
- Public API exports are surfaced from `main.lisp`
- Changes to evaluator semantics should be verified by tests, by the contracts, and by direct `cl-mcp` runtime evaluation

## Testing

Preferred:

- Run the full suite with `run-tests` on system `wardlisp/tests`
- Run targeted tests with `run-tests` using specific test names when narrowing failures
- Load `cl-spec/check-it` and `wardlisp/specs`, then run `spec-check function=<fn>` and `spec-check symbol=<fn>` for each public function a change touches

Fallback:

- `rove wardlisp.asd`
- `ros run --load scripts/check-specs.lisp` (and `WARDLISP_SPECS_MODE=negative-control` for the mutant check)

Before finishing substantial changes:

- Reload with `load-system` (`wardlisp` with `clear_fasls: true`, then `wardlisp/specs`)
- Run relevant tests and contracts
- For evaluator or reader changes, verify behavior directly with `repl-eval` or `wardlisp:evaluate`
- Run `mallet src/*.lisp specs/*.lisp scripts/*.lisp`

## Contracts

- Read a function's contract before changing it, and take a baseline.
- Never weaken a contract to make a check pass; bring a conflict between a contract and a requested behaviour to the user first.
- Keep `docs/language-spec.md` and the contracts in agreement; each clause cites the section it comes from.
- Register a new Function Spec or Property in `contract-names` / `property-names` (`specs/runner.lisp`).
- Report `verification_gaps` as `spec-check` gives them.

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
