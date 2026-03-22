# WardLisp Sandbox Security Audit

Date: 2026-03-22

## Goal

Verify that WardLisp code cannot affect anything outside the sandbox. Find vulnerabilities, fix them, and add regression tests.

## Scope

- Language-level escape attempts (reader, evaluator, builtins)
- Resource exhaustion attacks (parser stack, huge tokens, argument counts)
- Structural code review of evaluator internals
- Document OS-level sandbox gaps (not yet implemented)

## Attack Categories

### A. Reader Layer

| Attack | Input | Expected |
|--------|-------|----------|
| Read-time eval | `#.(+ 1 2)` | parse-error |
| Function shorthand | `#'car` | parse-error |
| Vector literal | `#(1 2 3)` | parse-error |
| Backquote | `` `(,x) `` | parse-error or name-error |
| Package prefix | `cl:open`, `sb-ext:run-program` | parse-error |
| Keyword symbol | `:keyword` | parse-error |
| String literal | `"hello"` | parse-error (unsupported) |
| Pipe symbol | `\|weird symbol\|` | parse-error |

### B. Evaluator Layer

| Attack | Input | Expected |
|--------|-------|----------|
| eval | `(eval '(+ 1 2))` | name-error |
| apply | `(apply + '(1 2))` | name-error |
| funcall | `(funcall + 1 2)` | name-error |
| load | `(load "evil.lisp")` | name-error |
| compile | `(compile nil (lambda () 1))` | name-error |
| intern | `(intern "OPEN" "CL")` | name-error |
| Builtin overwrite | `(define + 42)` then `(+ 1 2)` | type-error or prevented |

### C. Resource Exhaustion

| Attack | Input | Expected |
|--------|-------|----------|
| Deep nesting | 1000-deep nested parens | parse-error, not CL stack overflow |
| Huge token | 1M char symbol | error or timeout |
| Many arguments | `(+ 1 2 3 ... 10000)` | fuel exhaustion |

### D. Structural Review

- Confirm no CL `eval`, user-controllable `funcall`, `compile`, `load` in evaluator
- Confirm builtin lambdas don't leak user data to CL internals
- Confirm `env-set!` is not reachable from WardLisp code

## Phases

1. **Attack execution**: Run each attack via `(wardlisp:evaluate ...)` in REPL, record results
2. **Fix vulnerabilities**: Patch reader.lisp, evaluator.lisp, builtins.lisp as needed
3. **Add tests**: Expand tests/safety-test.lisp with all attack cases
4. **Document findings**: Record security boundary and remaining gaps

## Out of Scope (Future Work)

- OS-level sandbox (seccomp, cgroup, namespace, rlimit)
- Process isolation per evaluation
- Wall-clock timeout enforcement (currently reserved in API but not enforced)
