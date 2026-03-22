# Sandbox Security Audit — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix 3 CL stack overflow vulnerabilities and add comprehensive sandbox escape regression tests.

**Architecture:** Add a nesting depth counter to the reader (`read-expr`), quote converter (`ast-to-value`), and evaluator (`wardlisp-eval`) — all three share the same `max-depth` limit from exec-ctx. Harden reader to reject additional special characters. Add ~20 new test cases.

**Tech Stack:** Common Lisp, SBCL, Rove (test framework)

---

## Audit Findings

| # | Category | Attack | Result | Severity |
|---|----------|--------|--------|----------|
| V1 | Resource | Deep nesting (~25K parens) crashes CL parser | `storage-condition` | **Critical** |
| V2 | Resource | Deep quoted list crashes `ast-to-value` | `storage-condition` | **Critical** |
| V3 | Resource | Deep `if` nesting crashes `wardlisp-eval` (special forms bypass depth tracking) | `storage-condition` | **Critical** |
| C1 | Reader | `"`, `` ` ``, `,`, `\`, `\|` allowed in atom chars | Cosmetic | Low |
| OK | Reader | `#.`, `#'`, `#(` blocked | `parse-error` | — |
| OK | Reader | `cl:open`, `:keyword` blocked | `parse-error` | — |
| OK | Evaluator | `eval`, `apply`, `funcall`, `load`, `intern` | `name-error` | — |
| OK | Evaluator | `set!`, `setf`, `setq` | `name-error` | — |
| OK | Evaluator | Builtin overwrite via `define` | Builtins in first frame, found first | — |
| OK | Evaluator | Builtin shadow via `let` | `type-error` (correct) | — |
| OK | Resource | Huge token (1M chars) | `name-error` | — |
| OK | Structural | No CL `eval`/`compile`/`load` reachable from user code | — | — |
| OK | Structural | `env-set!` exported but no special form exposes it | — | — |

Root cause of V1-V3: **unbounded CL-level recursion** in three places. WardLisp's `max-depth` only tracks function application depth, not special form or parser recursion.

---

### Task 1: Add nesting depth limit to reader

**Files:**
- Modify: `src/reader.lisp` — `read-expr`, `read-list`
- Test: `tests/safety-test.lisp`

**Step 1: Write the failing test**

Add to `tests/safety-test.lisp`:

```lisp
(deftest test-deep-nesting-parse-stops
  (ok (signals
       (eval-safe
        (concatenate 'string
          (make-string 10000 :initial-element #\()
          "1"
          (make-string 10000 :initial-element #\)))
        :fuel 1000000)
       'wardlisp-parse-error)))
```

**Step 2: Run test to verify it fails**

Run: `(rove:run-test 'wardlisp/tests/safety-test::test-deep-nesting-parse-stops)`
Expected: FAIL (currently crashes with `storage-condition` instead of signaling `wardlisp-parse-error`)

**Step 3: Implement nesting depth limit in reader**

Modify `read-expr` and `read-list` to accept and pass a `depth` parameter (default 0). In `read-list`, increment depth and check against a `+max-parse-depth+` constant (1000). Signal `wardlisp-parse-error` when exceeded. Update `wardlisp-read` and `wardlisp-read-program` to pass initial depth 0.

```lisp
(defconstant +max-parse-depth+ 1000)

(defun read-expr (input pos &optional (depth 0))
  ;; ... existing code ...
  ;; In the #\( branch:
  ((char= ch #\() (read-list input (1+ pos) (1+ depth)))
  ;; In the #\' branch:
  ((char= ch #\') (read-quote input (1+ pos) depth))
  ;; ...)

(defun read-list (input pos &optional (depth 0))
  (when (> depth +max-parse-depth+)
    (error 'wardlisp-parse-error
           :message (format nil "Nesting depth ~d exceeds limit ~d" depth +max-parse-depth+)))
  ;; ... existing loop, passing depth to read-expr ...
```

**Step 4: Run test to verify it passes**

Run: `(rove:run-test 'wardlisp/tests/safety-test::test-deep-nesting-parse-stops)`
Expected: PASS

**Step 5: Commit**

```bash
git add src/reader.lisp tests/safety-test.lisp
git commit -m "fix: add nesting depth limit to reader to prevent CL stack overflow"
```

---

### Task 2: Add nesting depth limit to ast-to-value

**Files:**
- Modify: `src/evaluator.lisp` — `ast-to-value`, `eval-quote`
- Test: `tests/safety-test.lisp`

**Step 1: Write the failing test**

```lisp
(deftest test-deep-quoted-list-stops
  (ok (signals
       (eval-safe
        (concatenate 'string
          "'("
          (make-string 10000 :initial-element #\()
          "1"
          (make-string 10000 :initial-element #\))
          ")")
        :fuel 1000000 :max-cons 1000000)
       'wardlisp-error)))
```

**Step 2: Run test to verify it fails**

Expected: FAIL (crashes with `storage-condition`)

**Step 3: Add depth tracking to ast-to-value**

Add a `depth` parameter to `ast-to-value`. Increment on recursive cons-cell conversion. Check against `+max-parse-depth+` (reuse the reader constant, or define a shared one in types.lisp).

```lisp
(defun ast-to-value (ast ctx &optional (depth 0))
  (when (> depth +max-parse-depth+)
    (error 'wardlisp-parse-error
           :message "Quoted expression nesting too deep"))
  (cond
    ((null ast) nil)
    ((integerp ast) (check-integer ctx ast))
    ((eq ast t) t)
    ((stringp ast) ast)
    ((consp ast)
     (track-cons ctx)
     (make-ocons (ast-to-value (car ast) ctx (1+ depth))
                 (ast-to-value (cdr ast) ctx (1+ depth))))
    (t ast)))
```

**Step 4: Run test to verify it passes**

Expected: PASS

**Step 5: Commit**

```bash
git add src/evaluator.lisp tests/safety-test.lisp
git commit -m "fix: add depth limit to ast-to-value to prevent stack overflow on deep quotes"
```

---

### Task 3: Track expression depth in wardlisp-eval for special forms

**Files:**
- Modify: `src/types.lisp` — add `exec-ctx-expr-depth` slot and `track-expr-depth` helper
- Modify: `src/evaluator.lisp` — call `track-expr-depth` in `wardlisp-eval`
- Test: `tests/safety-test.lisp`

**Step 1: Write the failing test**

```lisp
(deftest test-deep-if-nesting-stops
  (ok (signals
       (eval-safe
        (let ((code (format nil "~{~a~}1~{~a~}"
                     (loop repeat 10000 collect "(if t ")
                     (loop repeat 10000 collect ")"))))
          code)
        :fuel 1000000)
       'wardlisp-error)))
```

**Step 2: Run test to verify it fails**

Expected: FAIL (crashes with `storage-condition`)

**Step 3: Add expression depth tracking**

In `types.lisp`, add `expr-depth` and `max-expr-depth` slots to `exec-ctx` (reuse `max-depth` for the limit or add a separate one). Add a `track-expr-depth` helper.

In `wardlisp-eval`, call depth tracking on every compound expression entry/exit:

```lisp
(defun wardlisp-eval (expr env ctx)
  (consume-fuel ctx)
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
    (t (error 'wardlisp-internal-error ...))))
```

`track-expr-depth` checks against `max-depth` and signals `wardlisp-recursion-limit-exceeded` when exceeded.

**Step 4: Run test to verify it passes**

Expected: PASS

**Step 5: Run full test suite**

Run: `rove wardlisp.asd`
Expected: All tests pass (existing depth tracking via `track-depth` on function calls is now supplemented, not replaced)

**Step 6: Commit**

```bash
git add src/types.lisp src/evaluator.lisp tests/safety-test.lisp
git commit -m "fix: track expression depth in evaluator to prevent stack overflow on nested special forms"
```

---

### Task 4: Harden reader atom-char-p (cosmetic)

**Files:**
- Modify: `src/reader.lisp` — `atom-char-p`
- Test: `tests/safety-test.lisp`

**Step 1: Write failing tests**

```lisp
(deftest test-reject-special-characters
  (ok (signals (eval-safe "\"hello\"") 'wardlisp-parse-error))
  (ok (signals (eval-safe "`x") 'wardlisp-parse-error))
  (ok (signals (eval-safe ",x") 'wardlisp-parse-error))
  (ok (signals (eval-safe "\\x") 'wardlisp-parse-error))
  (ok (signals (eval-safe "|x|") 'wardlisp-parse-error)))
```

**Step 2: Run tests to verify they fail**

Expected: FAIL (currently these parse as symbols)

**Step 3: Add exclusions to atom-char-p**

```lisp
(defun atom-char-p (ch)
  (and (not (char= ch #\())
       (not (char= ch #\)))
       (not (char= ch #\'))
       (not (char= ch #\;))
       (not (char= ch #\#))
       (not (char= ch #\"))
       (not (char= ch #\`))
       (not (char= ch #\,))
       (not (char= ch #\\))
       (not (char= ch #\|))
       (not (whitespace-p ch))))
```

**Step 4: Run full test suite**

Run: `rove wardlisp.asd`
Expected: All pass

**Step 5: Commit**

```bash
git add src/reader.lisp tests/safety-test.lisp
git commit -m "fix: reject special characters in reader atom-char-p"
```

---

### Task 5: Add comprehensive sandbox regression tests

**Files:**
- Modify: `tests/safety-test.lisp`

**Step 1: Add all remaining attack cases as tests**

```lisp
;; Reader attacks
(deftest test-reader-blocks-hash-dot (ok (signals (eval-safe "#.(+ 1 2)") 'wardlisp-parse-error)))
(deftest test-reader-blocks-hash-quote (ok (signals (eval-safe "#'car") 'wardlisp-parse-error)))
(deftest test-reader-blocks-hash-paren (ok (signals (eval-safe "#(1 2 3)") 'wardlisp-parse-error)))
(deftest test-reader-blocks-package-prefix (ok (signals (eval-safe "cl:open") 'wardlisp-parse-error)))
(deftest test-reader-blocks-keyword (ok (signals (eval-safe ":keyword") 'wardlisp-parse-error)))

;; Evaluator escape attempts
(deftest test-no-apply (ok (signals (eval-safe "(apply + '(1 2))") 'wardlisp-name-error)))
(deftest test-no-funcall (ok (signals (eval-safe "(funcall + 1 2)") 'wardlisp-name-error)))
(deftest test-no-load (ok (signals (eval-safe "(load \"evil\")") 'wardlisp-error)))
(deftest test-no-compile (ok (signals (eval-safe "(compile nil)") 'wardlisp-name-error)))
(deftest test-no-intern (ok (signals (eval-safe "(intern \"X\")") 'wardlisp-name-error)))

;; Builtin integrity
(deftest test-builtin-not-overwritable
  (ok (eql 3 (eval-safe "(define + 42) (+ 1 2)" :fuel 1000))))
```

**Step 2: Run full test suite**

Run: `rove wardlisp.asd`
Expected: All pass

**Step 3: Commit**

```bash
git add tests/safety-test.lisp
git commit -m "test: add comprehensive sandbox escape regression tests"
```

---

### Task 6: Lint, final test, push

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
