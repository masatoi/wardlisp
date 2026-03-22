# Tail Call Optimization (Trampoline) — Design & Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add tail-recursion optimization via trampoline so recursive WardLisp programs don't hit the CL call-depth limit for tail-recursive patterns like `(define (loop n acc) (if (= n 0) acc (loop (- n 1) (+ acc n))))`.

**Architecture:** Introduce a `tail-call` struct. When `apply-function` detects a closure call, it returns a `tail-call` instead of recursing. A trampoline loop in `wardlisp-eval` unwinds successive `tail-call` values in constant CL stack. Special forms (`if`, `let`, `begin`, `cond`, `and`, `or`, `define`) are transparent — they already return whatever the tail-position `wardlisp-eval` returns, so `tail-call` structs propagate naturally.

**Tech Stack:** Common Lisp, SBCL, Rove (test framework)

---

## Design

### Tail Position Definition

An expression is in **tail position** if its value is returned directly without further computation:

| Form | Tail position(s) |
|------|-------------------|
| `(if test then else)` | `then` and `else` branches |
| `(let (...) body...)` | Last expression of `body` |
| `(begin e1 e2 ... en)` | `en` (last expression) |
| `(cond (t1 e1) ...)` | Each `e1` result expression |
| `(and a b c)` | `c` (last argument) |
| `(or a b c)` | `c` (last argument) |
| `(define ...)` | Value expression / last body expression |
| Function body | The body expression |

### Trampoline Mechanism

1. `apply-function` for closures: instead of calling `wardlisp-eval(body, new-env, ctx)`, return `(make-tail-call :expr body :env new-env)`
2. `wardlisp-eval` gets a trampoline loop: after evaluating an expression, if the result is a `tail-call`, loop back and evaluate `tail-call-expr` in `tail-call-env`
3. Depth tracking (`track-depth`) moves into the trampoline loop — increment on entry, decrement when the loop finishes (not per iteration)

### What Changes

**types.lisp:**
- New `tail-call` struct with `expr` and `env` fields
- Export `tail-call`, `make-tail-call`, `tail-call-p`, `tail-call-expr`, `tail-call-env`

**evaluator.lisp:**
- `wardlisp-eval`: add trampoline loop around compound eval results
- `apply-function`: return `make-tail-call` instead of calling `wardlisp-eval` for closures
- `apply-function`: depth tracking wraps the whole trampoline, not single calls

### What Doesn't Change

- Special forms (`eval-if`, `eval-let`, `eval-begin`, `eval-cond`, `eval-and`, `eval-or`): they already return the result of `wardlisp-eval` in tail position, so `tail-call` structs propagate naturally
- Reader, builtins, env, main — no changes needed

---

### Task 1: Add tail-call struct to types.lisp

**Files:**
- Modify: `src/types.lisp`

**Step 1: Add the tail-call struct**

Insert after the `closure` struct definition:

```lisp
(defstruct (tail-call (:constructor make-tail-call (&key expr env)))
  "Represents a pending tail call for trampoline evaluation."
  expr
  env)
```

**Step 2: Add exports**

Add to the package exports: `tail-call`, `make-tail-call`, `tail-call-p`, `tail-call-expr`, `tail-call-env`

**Step 3: Verify compilation**

Run: `(asdf:compile-system :wardlisp :force t)` via repl-eval
Expected: No warnings

**Step 4: Commit**

```bash
git add src/types.lisp
git commit -m "feat: add tail-call struct for trampoline TCO"
```

---

### Task 2: Write failing test for tail recursion

**Files:**
- Modify: `tests/integration-test.lisp`

**Step 1: Write the failing test**

Add to `tests/integration-test.lisp`:

```lisp
;;; Tail call optimization
(deftest test-tail-recursive-sum
  (let ((result (evaluate "
    (define (sum-iter n acc)
      (if (= n 0) acc
          (sum-iter (- n 1) (+ acc n))))
    (sum-iter 10000 0)"
    :fuel 1000000 :max-depth 200)))
    (ok (= 50005000 result))))

(deftest test-tail-recursive-no-stack-overflow
  (let ((result (evaluate "
    (define (count-down n)
      (if (= n 0) 0
          (count-down (- n 1))))
    (count-down 50000)"
    :fuel 1000000 :max-depth 200)))
    (ok (= 0 result))))
```

**Step 2: Run tests to verify they fail**

Run: `rove wardlisp.asd` (from bash — clean process)
Expected: FAIL — `sum-iter 10000` and `count-down 50000` will hit `recursion-limit-exceeded` at depth 200

**Step 3: Commit**

```bash
git add tests/integration-test.lisp
git commit -m "test: add failing tests for tail recursion optimization"
```

---

### Task 3: Implement trampoline in wardlisp-eval and apply-function

**Files:**
- Modify: `src/evaluator.lisp`

**Step 1: Modify apply-function to return tail-call for closures**

Replace the closure branch in `apply-function`. Instead of calling `wardlisp-eval`, return a `tail-call`:

```lisp
(defun apply-function (func args ctx)
  "Apply FUNC to ARGS. Returns tail-call struct for closures (trampolined)."
  (consume-fuel ctx 4)
  (cond
    ((closure-p func)
     (let ((params (closure-params func)))
       (when (/= (length params) (length args))
         (error 'wardlisp-arity-error
                :message (format nil "~a expects ~d args, got ~d"
                                 (or (closure-name func) "lambda")
                                 (length params) (length args))))
       (let ((call-env (env-extend (closure-env func) params args)))
         (make-tail-call :expr (closure-body func) :env call-env))))
    ((builtin-p func)
     (when (and (builtin-arity func)
                (/= (builtin-arity func) (length args)))
       (error 'wardlisp-arity-error
              :message (format nil "~a expects ~d args, got ~d"
                               (builtin-name func)
                               (builtin-arity func) (length args))))
     (funcall (builtin-func func) args ctx))
    (t (error 'wardlisp-type-error
              :message (format nil "Not a function: ~s" func)))))
```

Key change: no `track-depth` here, no `wardlisp-eval` call. Just return the `tail-call`.

**Step 2: Add trampoline loop to wardlisp-eval**

Replace `wardlisp-eval` with a trampoline version:

```lisp
(defun wardlisp-eval (expr env ctx)
  "Evaluate EXPR in ENV with execution context CTX.
Implements trampoline for tail call optimization."
  (track-depth ctx 1)
  (unwind-protect
       (loop
         (consume-fuel ctx)
         (let ((result
                 (cond
                   ((integerp expr) (check-integer ctx expr))
                   ((eq expr t) t)
                   ((null expr) nil)
                   ((stringp expr) (env-lookup env expr))
                   ((consp expr)
                    (track-expr-depth ctx 1)
                    (unwind-protect
                         (eval-compound expr env ctx)
                      (track-expr-depth ctx -1)))
                   (t (error 'wardlisp-internal-error
                             :message (format nil "Unknown expression type: ~s" expr))))))
           (if (tail-call-p result)
               (progn
                 (setf expr (tail-call-expr result))
                 (setf env (tail-call-env result)))
               (return result))))
    (track-depth ctx -1)))
```

Key changes:
- `track-depth` wraps the entire trampoline loop (one depth level per `wardlisp-eval` entry)
- When result is a `tail-call`, loop back with new `expr` and `env` (no CL stack growth)
- When result is NOT a `tail-call`, return it

**Step 3: Run tests to verify they pass**

Run: `rove wardlisp.asd`
Expected: ALL tests pass — both new tail-recursion tests and all existing tests

**Step 4: Commit**

```bash
git add src/evaluator.lisp
git commit -m "feat: implement trampoline-based tail call optimization"
```

---

### Task 4: Add more TCO tests and edge cases

**Files:**
- Modify: `tests/integration-test.lisp`

**Step 1: Add edge case tests**

```lisp
(deftest test-tail-call-in-let
  (let ((result (evaluate "
    (define (f n)
      (let ((x (+ n 1)))
        (if (= x 100) x (f x))))
    (f 0)"
    :fuel 100000 :max-depth 50)))
    (ok (= 100 result))))

(deftest test-tail-call-in-begin
  (let ((result (evaluate "
    (define (f n)
      (begin
        (+ 1 1)
        (if (= n 0) 42 (f (- n 1)))))
    (f 10000)"
    :fuel 1000000 :max-depth 50)))
    (ok (= 42 result))))

(deftest test-tail-call-in-cond
  (let ((result (evaluate "
    (define (classify n)
      (cond ((= n 0) 0)
            ((< n 0) (classify (+ n 1)))
            (t (classify (- n 1)))))
    (classify 5000)"
    :fuel 1000000 :max-depth 50)))
    (ok (= 0 result))))

(deftest test-mutual-like-tail-recursion
  (let ((result (evaluate "
    (define (even? n)
      (if (= n 0) t (odd? (- n 1))))
    (define (odd? n)
      (if (= n 0) nil (even? (- n 1))))
    (even? 1000)"
    :fuel 1000000 :max-depth 50)))
    (ok (eq t result))))

(deftest test-non-tail-recursion-still-works
  (let ((result (evaluate "
    (define (fact n)
      (if (= n 0) 1 (* n (fact (- n 1)))))
    (fact 10)")))
    (ok (= 3628800 result))))
```

**Step 2: Run full test suite**

Run: `rove wardlisp.asd`
Expected: All pass

**Step 3: Commit**

```bash
git add tests/integration-test.lisp
git commit -m "test: add TCO edge case tests (let, begin, cond, mutual recursion)"
```

---

### Task 5: Lint, final test, push

**Step 1: Lint**

Run: `mallet src/*.lisp`
Expected: No errors

**Step 2: Force recompile**

Run: `(asdf:compile-system :wardlisp :force t)`
Expected: No warnings

**Step 3: Full test suite**

Run: `rove wardlisp.asd`
Expected: All pass

**Step 4: Push**

```bash
git push origin main
```
