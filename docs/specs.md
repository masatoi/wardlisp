# wardlisp/specs: executable contracts for wardlisp's public API

`wardlisp/specs` is an opt-in bundle of [cl-spec](https://github.com/masatoi/cl-spec)
Function Specs and Properties about wardlisp's public API, a runner that checks
them through cl-spec's Lisp API, and a CI job (`specs` in
`.github/workflows/ci.yml`) that runs it on every push and pull request.

Each clause is derived from `docs/language-spec.md` and cites its section. The
contracts are the machine-checked half of that document: when the two disagree,
one of them is wrong, and which one is a question for the maintainer, not
something to settle by editing whichever is easier.

A passing check is evidence about the inputs that were generated, under the
seeds and budgets that ran. It is not a proof.

## Layout

| File | Contents |
|---|---|
| `specs/package.lisp` | package `wardlisp/specs` |
| `specs/generators.lisp` | data, program and argument generators; named specs; the documented error types and metrics shape |
| `specs/api.lisp` | Function Specs, one per public function, registered on the production symbol |
| `specs/language.lisp` | Properties: relations across calls (limits, arithmetic, round trips, equality) |
| `specs/runner.lisp` | `run-bundle`, `run-negative-control`, `main` |
| `scripts/check-specs.lisp` | command-line entry; what CI runs |

`wardlisp` never depends on `wardlisp/specs`, so production loads need no cl-spec.

## What is covered

| Function | Function Spec | Properties |
|---|---|---|
| `evaluate` | never signals; metrics have the shape of §13.2; success has no error, failure returns `nil` with a documented error type and a message (§12, §13); never reports `internal-error`, so no host error escapes (§1, §12); `steps-used + fuel-remaining = fuel` (§11.1); output never exceeds `max-output`; a success stays inside every limit; a step/memory limit error is witnessed by its counter; no number in a result exceeds `max-integer` (§3, §11.4). Covers calls that pass every limit explicitly. | `evaluate-reports-invalid-arguments`, `evaluate-default-fuel-is-documented`, `evaluate-sees-host-bindings`, `evaluate-is-deterministic`, `step-limit-is-exact`, `memory-limit-is-exact`, `recursion-limit-is-exact`, `output-limit-is-exact`, `integer-arithmetic-agrees-with-common-lisp`, `out-of-range-float-literals-are-parse-errors`, `float-overflow-is-a-number-limit`, `quotient-and-mod-truncate-toward-zero`, `quoted-data-round-trip-through-print-value`, `equal?-is-structural-equality`, `evaluate-results-carry-host-strings` |
| `print-value` | six cases by type (§10, §17.3): integer in decimal; float with a point, no exponent, reads back; `t`/`nil`; symbol bare; string quoted and reads back; list in parentheses | `quoted-data-round-trip-through-print-value`, `equal?-is-structural-equality` |
| `make-string-value` | a CL string gives a string value with equal contents and its own storage; anything else signals `wardlisp-type-error` (§17.8) | `host-strings-are-copied-both-ways`, `evaluate-results-carry-host-strings` |
| `string-value` | a string value gives a fresh CL string of its contents; anything else signals `wardlisp-type-error` (§17.8) | the same two |
| `string-value-p`, `symbol-value-p`, `number-value-p` | a boolean, true exactly for their type (§17.8) | `value-classifiers-partition-data` |
| `ocons-p` | none | `value-classifiers-partition-data` |

Function Specs are registered on the production symbols (`print-value` lives in
`wardlisp/src/builtins` and is re-exported from `wardlisp`). Properties are in
`wardlisp/specs`.

The `*-limit-is-exact` properties state §11's boundary rule directly: a program
that uses N of a resource succeeds with limit N and fails with the matching
error with limit N - 1. They hold vacuously for a generated program that fails
anyway or uses fewer than two units; the `limit-program` generator is weighted
so that most trials do use every resource.

## Running

### Through cl-mcp (the cl-spec tool group)

```text
load-system  cl-spec/check-it
load-system  wardlisp/specs
spec-list
spec-symbol    symbol=wardlisp:evaluate
spec-describe  kind=function-spec name=wardlisp:evaluate
spec-check     function=wardlisp:evaluate trials=200
spec-check     symbol=wardlisp:evaluate
```

`symbol=` runs only the Properties `(:about ...)` a function; `function=` runs
only its Function Spec. Run both. See `prompts/cl-spec-driven-development.md`.

### From the command line, as CI does

```bash
ros run --load scripts/check-specs.lisp
WARDLISP_SPECS_MODE=negative-control ros run --load scripts/check-specs.lisp
```

`WARDLISP_SPECS_REPORT=<file>` writes the report as one Lisp form. Exit status
is 0 when everything passed, 1 when a check failed, 2 when the checks could not
run (load error, unknown mode, or wardlisp loaded from another checkout).

The runner checks every target under the seeds `1`, `42` and `20261012`,
Properties under profile `:normal` and Function Specs with 200 trials. A run
counts as passed only with status `:passed`, a positive trial count, a measured
rejection count below it (Function Specs), and every declared case reached. It
also fails when a listed target is not registered or a registered Property of
the package is missing from `property-names`.

### Negative control

`negative-control` swaps known-wrong implementations in, one at a time, and
requires that each is caught under every seed:

| Mutant | Must be caught by |
|---|---|
| `print-wstring` that does not escape | `print-value` contract, `quoted-data-round-trip-through-print-value` |
| `make-string-value` that shares the host's storage | `make-string-value` contract, `host-strings-are-copied-both-ways` |
| `consume-fuel` that stops one step early | `step-limit-is-exact` |
| `track-cons` that refuses the allocation reaching the limit | `memory-limit-is-exact` |
| `mod` computed with `cl:mod` (floor) | `quotient-and-mod-truncate-toward-zero` |
| `parse-float-token` that lets the host reader's error through | `out-of-range-float-literals-are-parse-errors` |
| `call-checking-float-range` that lets a float overflow through | `float-overflow-is-a-number-limit` |

It replaces production functions while it runs; use it only in a process of
its own.

## Changing a contract

Do not weaken a contract to make a check pass. A contract that contradicts the
requirement is reported with the requirement, the clause and the counterexample,
and changed only after the maintainer agrees. When a change is agreed, update
`docs/language-spec.md` and the contract together, add the name to
`contract-names` or `property-names` in `specs/runner.lisp`, and, for a new
mutant, to `*mutants*`.

cl-spec is pinned in CI by `CL_SPEC_REVISION`. Bump it deliberately, in its own
commit, after running the bundle against the new revision.

## Known gaps

- Of `evaluate`'s defaults only `fuel` (1,000,000) is checked; the `evaluate`
  contract itself covers calls that pass every limit.
- `timeout-exceeded` is never reached by the generated programs, which are
  bounded by small fuel.
- `print-value` prints a pair `(3 . 4)` and the three-element list of `3`, the
  symbol `.` and `4` identically, and the reader has no dotted-pair syntax, so
  the round-trip property is limited to proper lists of symbols that start with
  a letter.
- Closures, builtins, `timeout`, and `random` without a seed are not covered.
- Floats are generated only up to 1000 in magnitude with three decimals.
- Input-domain coverage is not measured.
