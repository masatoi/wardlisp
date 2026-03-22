# WardLisp

A restricted Lisp dialect designed for safe server-side evaluation in educational Lisp-learning games. Written in Common Lisp.

## Why

Running user-submitted Lisp code on a server is dangerous. Infinite loops exhaust CPU, unbounded allocation eats memory, and a single `(open "/etc/passwd")` breaks everything. WardLisp solves this by defining a minimal, sandboxed Lisp that preserves the essence of Lisp (S-expressions, recursion, higher-order functions, closures) while making it impossible to escape the evaluator.

## Design Priorities

1. **Safety** -- Zero access to the host system
2. **Resource control** -- Hard limits on computation, memory, and output
3. **Education** -- Teach the core of Lisp: S-expressions, recursion, higher-order functions
4. **Determinism** -- Same input always produces the same result

## Language Overview

WardLisp is a Lisp-1 (like Scheme: functions and variables share a single namespace). It is intentionally *not* Common Lisp compatible.

### Data Types

| Type | Examples |
|------|---------|
| Integer | `42`, `-7`, `0` |
| Boolean | `t`, `nil` |
| Pair / List | `(cons 1 2)`, `'(1 2 3)` |
| Closure | `(lambda (x) (+ x 1))` |
| Builtin | `+`, `cons`, `car` |

### Special Forms

`quote`, `if`, `let` (sequential binding), `lambda`, `define`, `cond`, `and`, `or`, `begin`, `apply`

### Builtins

- **Arithmetic**: `+`, `-`, `*`, `div`, `mod`
- **Comparison**: `=`, `<`, `<=`, `>`, `>=`
- **Lists**: `cons`, `car`, `cdr`, `list`, `null?`, `atom?`, `length`, `append`
- **Equality**: `eq?` (shallow), `equal?` (deep structural)
- **Other**: `not`, `print`

### Tail Call Optimization

Tail calls in supported positions (if branches, let/begin body, cond clauses, and/or last argument) are optimized via trampoline. Tail-recursive functions run in constant stack space regardless of iteration count.

```scheme
(define (sum-iter n acc)
  (if (= n 0) acc
      (sum-iter (- n 1) (+ acc n))))
(sum-iter 50000 0)
;=> 1250025000
```

### What's Intentionally Missing

No macros, no mutation (`set!`/`setf`), no loops, no `eval`, no I/O, no packages, no reader macros, no FFI, no threads, no randomness.

## Resource Limits

Every evaluation runs under hard limits. Exceeding any limit immediately halts execution with a specific error.

| Resource | Default | Error |
|----------|---------|-------|
| Computation steps (fuel) | 10,000 | `step-limit-exceeded` |
| Recursion depth | 100 | `recursion-limit-exceeded` |
| Cons cells allocated | 10,000 | `memory-limit-exceeded` |
| Integer absolute value | 2^64 | `integer-limit-exceeded` |
| Print output | 1,000 chars | `output-limit-exceeded` |

## Usage

```lisp
(ql:quickload :wardlisp)

;; Basic evaluation
(wardlisp:evaluate "(+ 1 2)")
;; => 3
;; => (:steps-used 7 :max-depth-reached 0 :cons-allocated 0 ...)

;; With custom limits
(wardlisp:evaluate "(fact 20)"
  :fuel 100000
  :max-depth 200)

;; Resource exhaustion is safe
(wardlisp:evaluate "((lambda (f) (f f)) (lambda (f) (f f)))")
;; => NIL
;; => (:error-type :step-limit-exceeded :error-message "..." ...)
```

The `evaluate` function returns two values: the result (or `NIL` on error) and a metrics plist containing `:steps-used`, `:max-depth-reached`, `:cons-allocated`, `:output`, `:fuel-remaining`, `:error-type`, and `:error-message`.

## Examples

```scheme
;; Factorial
(define (fact n)
  (if (= n 0) 1
      (* n (fact (- n 1)))))
(fact 10)
;=> 3628800

;; Map over a list
(define (my-map f lst)
  (if (null? lst) '()
      (cons (f (car lst)) (my-map f (cdr lst)))))
(my-map (lambda (x) (* x x)) '(1 2 3 4 5))
;=> (1 4 9 16 25)

;; Closures
(define (make-adder n)
  (lambda (x) (+ n x)))
(let ((add5 (make-adder 5)))
  (add5 10))
;=> 15

;; Deep equality for grading
(define (insert x lst)
  (cond ((null? lst) (list x))
        ((<= x (car lst)) (cons x lst))
        (t (cons (car lst) (insert x (cdr lst))))))
(define (my-sort lst)
  (if (null? lst) nil
      (insert (car lst) (my-sort (cdr lst)))))
(equal? (my-sort '(3 1 4 1 5 9 2 6)) '(1 1 2 3 4 5 6 9))
;=> t

;; Apply
(apply + '(1 2 3))
;=> 6
```

## Project Structure

```
src/          Core implementation
  types.lisp      Data types (integer, pair, closure, builtin, etc.)
  reader.lisp     Tokenizer and S-expression parser
  env.lisp        Lexical environment with frame chains
  builtins.lisp   Built-in functions (+, -, cons, car, ...)
  evaluator.lisp  Tree-walking evaluator with resource tracking
  main.lisp       Public API (evaluate, print-value)
tests/        Rove test suites
docs/         Language specification and PRD
```

## Development

Requires [SBCL](http://www.sbcl.org/) and [Roswell](https://roswell.github.io/) (recommended).

```bash
# Run tests
rove wardlisp.asd

# Lint
mallet src/*.lisp
```

## License

See LICENSE file.
