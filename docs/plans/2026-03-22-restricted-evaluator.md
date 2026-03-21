# Restricted Lisp Evaluator Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement a safe, restricted Lisp evaluator for educational games with step counting, recursion limits, and memory controls per `docs/prd.md`.

**Architecture:** Tree-walking interpreter with a safe custom reader (not CL `read`). The reader produces an AST using CL native values (integers, strings for symbol names, CL lists for structure). Runtime values include custom `ocons` structs (for cons-cell counting) and `closure` structs. A mutable `exec-ctx` struct tracks fuel, recursion depth, and cons allocations. Lexical environments are linked alists. All errors are CL conditions under a common `omoikane-error` hierarchy.

**Tech Stack:** Common Lisp (SBCL), ASDF (package-inferred-system), Rove (testing)

**Data Flow:**
```
User Code (string)
  → Reader (src/reader.lisp) → AST (CL lists/strings/integers)
  → Evaluator (src/evaluator.lisp) → Values (integers, T/NIL, ocons, closure)
  → Public API (src/main.lisp) → Result + Metrics
```

**Module Dependencies:**
```
types.lisp       (no deps — structs, conditions, exec-ctx)
reader.lisp      → types
env.lisp         → types
builtins.lisp    → types, env
evaluator.lisp   → types, reader, env, builtins
main.lisp        → evaluator (re-exports public API)
```

---

### Task 1: Project Structure & Core Types

**Files:**
- Modify: `omoikane-lisp.asd`
- Create: `src/types.lisp`
- Modify: `src/main.lisp`
- Create: `tests/types-test.lisp`
- Modify: `tests/main.lisp`

**Step 1: Update omoikane-lisp.asd for new module structure**

```lisp
(defsystem "omoikane-lisp"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()
  :serial t
  :components ((:module "src"
                :serial t
                :components
                ((:file "types")
                 (:file "reader")
                 (:file "env")
                 (:file "builtins")
                 (:file "evaluator")
                 (:file "main"))))
  :description "Restricted Lisp evaluator for educational games"
  :in-order-to ((test-op (test-op "omoikane-lisp/tests"))))

(defsystem "omoikane-lisp/tests"
  :author ""
  :license ""
  :depends-on ("omoikane-lisp"
               "rove")
  :serial t
  :components ((:module "tests"
                :serial t
                :components
                ((:file "types-test")
                 (:file "reader-test")
                 (:file "env-test")
                 (:file "evaluator-test")
                 (:file "safety-test")
                 (:file "integration-test"))))
  :description "Test system for omoikane-lisp"
  :perform (test-op (op c) (symbol-call :rove :run c)))
```

**Step 2: Create src/types.lisp with structs and conditions**

```lisp
(defpackage :omoikane-lisp/src/types
  (:use :cl)
  (:export
   ;; Cons cell
   #:ocons #:make-ocons #:ocons-p #:ocons-ocar #:ocons-ocdr
   ;; Closure
   #:closure #:make-closure #:closure-p
   #:closure-params #:closure-body #:closure-env #:closure-name
   ;; Builtin
   #:builtin #:make-builtin #:builtin-p
   #:builtin-name #:builtin-func #:builtin-arity
   ;; Execution context
   #:exec-ctx #:make-exec-ctx
   #:exec-ctx-fuel #:exec-ctx-max-depth #:exec-ctx-current-depth
   #:exec-ctx-cons-count #:exec-ctx-max-cons
   #:exec-ctx-output #:exec-ctx-max-output
   #:exec-ctx-steps-used #:exec-ctx-max-depth-reached
   #:exec-ctx-max-integer
   ;; Conditions
   #:omoikane-error #:omoikane-error-message
   #:omoikane-parse-error
   #:omoikane-name-error
   #:omoikane-type-error
   #:omoikane-arity-error
   #:omoikane-step-limit-exceeded
   #:omoikane-recursion-limit-exceeded
   #:omoikane-memory-limit-exceeded
   #:omoikane-integer-limit-exceeded
   #:omoikane-output-limit-exceeded
   #:omoikane-timeout-exceeded
   #:omoikane-internal-error
   ;; Helpers
   #:consume-fuel #:track-depth #:track-cons #:check-integer))
(in-package :omoikane-lisp/src/types)

;;; --- Custom cons cell for allocation counting ---

(defstruct (ocons (:constructor make-ocons (ocar ocdr)))
  "A cons cell in the restricted language. Separate from CL cons for counting."
  ocar
  ocdr)

;;; --- Closure ---

(defstruct (closure (:constructor make-closure (params body env &optional name)))
  "A user-defined function closure."
  (params nil :type list)          ; list of param name strings
  body                              ; AST body expression
  env                               ; captured lexical environment
  (name nil))                       ; optional name for recursion

;;; --- Builtin function ---

(defstruct (builtin (:constructor make-builtin (name func arity)))
  "A built-in function."
  (name "" :type string)
  func                              ; CL function (lambda (args ctx) ...)
  (arity nil))                      ; nil = variadic, integer = exact count

;;; --- Execution context ---

(defstruct (exec-ctx (:constructor make-exec-ctx
                         (&key (fuel 10000) (max-depth 100)
                               (max-cons 10000) (max-output 1000)
                               (max-integer (expt 2 64)))))
  "Mutable execution context tracking resource consumption."
  (fuel 10000 :type integer)
  (max-depth 100 :type fixnum)
  (current-depth 0 :type fixnum)
  (cons-count 0 :type fixnum)
  (max-cons 10000 :type fixnum)
  (output (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)
          :type (array character (*)))
  (max-output 1000 :type fixnum)
  (steps-used 0 :type integer)
  (max-depth-reached 0 :type fixnum)
  (max-integer (expt 2 64) :type integer))

;;; --- Error conditions ---

(define-condition omoikane-error (error)
  ((message :initarg :message :reader omoikane-error-message
            :initform ""))
  (:report (lambda (c s) (format s "~a" (omoikane-error-message c)))))

(define-condition omoikane-parse-error (omoikane-error) ())
(define-condition omoikane-name-error (omoikane-error) ())
(define-condition omoikane-type-error (omoikane-error) ())
(define-condition omoikane-arity-error (omoikane-error) ())
(define-condition omoikane-step-limit-exceeded (omoikane-error) ())
(define-condition omoikane-recursion-limit-exceeded (omoikane-error) ())
(define-condition omoikane-memory-limit-exceeded (omoikane-error) ())
(define-condition omoikane-integer-limit-exceeded (omoikane-error) ())
(define-condition omoikane-output-limit-exceeded (omoikane-error) ())
(define-condition omoikane-timeout-exceeded (omoikane-error) ())
(define-condition omoikane-internal-error (omoikane-error) ())

;;; --- Resource control helpers ---

(defun consume-fuel (ctx &optional (amount 1))
  "Consume fuel from context. Signals step-limit-exceeded when exhausted."
  (decf (exec-ctx-fuel ctx) amount)
  (incf (exec-ctx-steps-used ctx) amount)
  (when (<= (exec-ctx-fuel ctx) 0)
    (error 'omoikane-step-limit-exceeded
           :message (format nil "Step limit exceeded after ~d steps"
                            (exec-ctx-steps-used ctx)))))

(defun track-depth (ctx delta)
  "Adjust recursion depth. Signals recursion-limit-exceeded when too deep."
  (incf (exec-ctx-current-depth ctx) delta)
  (when (> (exec-ctx-current-depth ctx) (exec-ctx-max-depth-reached ctx))
    (setf (exec-ctx-max-depth-reached ctx) (exec-ctx-current-depth ctx)))
  (when (> (exec-ctx-current-depth ctx) (exec-ctx-max-depth ctx))
    (error 'omoikane-recursion-limit-exceeded
           :message (format nil "Recursion depth ~d exceeds limit ~d"
                            (exec-ctx-current-depth ctx)
                            (exec-ctx-max-depth ctx)))))

(defun track-cons (ctx &optional (count 1))
  "Track cons cell allocation. Signals memory-limit-exceeded when over."
  (incf (exec-ctx-cons-count ctx) count)
  (when (> (exec-ctx-cons-count ctx) (exec-ctx-max-cons ctx))
    (error 'omoikane-memory-limit-exceeded
           :message (format nil "Cons allocation ~d exceeds limit ~d"
                            (exec-ctx-cons-count ctx)
                            (exec-ctx-max-cons ctx)))))

(defun check-integer (ctx value)
  "Check integer is within allowed range. Signals integer-limit-exceeded if not."
  (when (> (abs value) (exec-ctx-max-integer ctx))
    (error 'omoikane-integer-limit-exceeded
           :message (format nil "Integer ~d exceeds limit ~d"
                            value (exec-ctx-max-integer ctx))))
  value)
```

**Step 3: Create stub files for other modules**

Create minimal stub files for `reader.lisp`, `env.lisp`, `builtins.lisp`, `evaluator.lisp` so the system loads. Each with just `(defpackage ...) (in-package ...)`.

**Step 4: Update src/main.lisp as the public API package**

```lisp
(defpackage :omoikane-lisp
  (:use :cl)
  (:export
   ;; Public API (filled in Task 9)
   #:evaluate))
(in-package :omoikane-lisp)
```

**Step 5: Write tests for types**

Create `tests/types-test.lisp`:
```lisp
(defpackage :omoikane-lisp/tests/types-test
  (:use :cl :rove :omoikane-lisp/src/types))
(in-package :omoikane-lisp/tests/types-test)

(deftest test-ocons-creation
  (let ((cell (make-ocons 1 2)))
    (ok (ocons-p cell))
    (ok (= 1 (ocons-ocar cell)))
    (ok (= 2 (ocons-ocdr cell)))))

(deftest test-closure-creation
  (let ((c (make-closure '("x") '("+" "x" 1) nil)))
    (ok (closure-p c))
    (ok (equal '("x") (closure-params c)))))

(deftest test-exec-ctx-defaults
  (let ((ctx (make-exec-ctx)))
    (ok (= 10000 (exec-ctx-fuel ctx)))
    (ok (= 100 (exec-ctx-max-depth ctx)))
    (ok (= 0 (exec-ctx-current-depth ctx)))))

(deftest test-consume-fuel
  (let ((ctx (make-exec-ctx :fuel 3)))
    (consume-fuel ctx)
    (ok (= 2 (exec-ctx-fuel ctx)))
    (consume-fuel ctx 2)
    (ok (signals (consume-fuel ctx) 'omoikane-step-limit-exceeded))))

(deftest test-track-depth
  (let ((ctx (make-exec-ctx :max-depth 2)))
    (track-depth ctx 1)
    (ok (= 1 (exec-ctx-current-depth ctx)))
    (track-depth ctx 1)
    (ok (signals (track-depth ctx 1) 'omoikane-recursion-limit-exceeded))))

(deftest test-track-cons
  (let ((ctx (make-exec-ctx :max-cons 2)))
    (track-cons ctx)
    (ok (= 1 (exec-ctx-cons-count ctx)))
    (track-cons ctx)
    (ok (signals (track-cons ctx) 'omoikane-memory-limit-exceeded))))

(deftest test-check-integer
  (let ((ctx (make-exec-ctx :max-integer 100)))
    (ok (= 50 (check-integer ctx 50)))
    (ok (signals (check-integer ctx 200) 'omoikane-integer-limit-exceeded))))
```

**Step 6: Delete old tests/main.lisp placeholder**

Remove `tests/main.lisp` (replaced by specific test files).

**Step 7: Load system and run tests**

```
load-system: omoikane-lisp
run-tests: omoikane-lisp/tests
```
Expected: All types-test tests pass.

**Step 8: Commit**

```bash
git add -A && git commit -m "feat: add core types, conditions, and execution context"
```

---

### Task 2: Safe Reader

**Files:**
- Modify: `src/reader.lisp`
- Create: `tests/reader-test.lisp`

The reader converts a string into an AST. It does NOT use CL's `read`. The AST uses:
- Integers → CL integers
- Symbol names → CL strings (e.g., `"+"`, `"factorial"`, `"null?"`)
- `t` → CL `T`
- `nil` → CL `NIL`
- Lists → CL lists
- `'x` → `("quote" x)`

**Step 1: Write reader tests**

```lisp
(defpackage :omoikane-lisp/tests/reader-test
  (:use :cl :rove :omoikane-lisp/src/reader))
(in-package :omoikane-lisp/tests/reader-test)

;; --- Atoms ---
(deftest test-read-integer
  (ok (= 42 (omoikane-read "42")))
  (ok (= -7 (omoikane-read "-7")))
  (ok (= 0 (omoikane-read "0"))))

(deftest test-read-boolean
  (ok (eq t (omoikane-read "t")))
  (ok (eq nil (omoikane-read "nil"))))

(deftest test-read-symbol
  (ok (equal "+" (omoikane-read "+")))
  (ok (equal "factorial" (omoikane-read "factorial")))
  (ok (equal "null?" (omoikane-read "null?"))))

;; --- Lists ---
(deftest test-read-list
  (ok (equal '("+" 1 2) (omoikane-read "(+ 1 2)")))
  (ok (equal nil (omoikane-read "()")))
  (ok (equal '("list" 1 2 3) (omoikane-read "(list 1 2 3)"))))

(deftest test-read-nested-list
  (ok (equal '("+" ("*" 2 3) 4) (omoikane-read "(+ (* 2 3) 4)"))))

;; --- Quote ---
(deftest test-read-quote
  (ok (equal '("quote" "x") (omoikane-read "'x")))
  (ok (equal '("quote" (1 2 3)) (omoikane-read "'(1 2 3)"))))

;; --- Whitespace & comments ---
(deftest test-read-whitespace
  (ok (= 42 (omoikane-read "  42  ")))
  (ok (equal '("+" 1 2) (omoikane-read " ( +  1  2 ) "))))

(deftest test-read-comment
  (ok (= 42 (omoikane-read "; this is a comment\n42"))))

;; --- Multiple expressions ---
(deftest test-read-program
  (ok (equal '(("define" ("f" "x") ("+" "x" 1)) ("f" 10))
             (omoikane-read-program "(define (f x) (+ x 1))\n(f 10)"))))

;; --- Errors ---
(deftest test-read-unmatched-paren
  (ok (signals (omoikane-read "(+ 1 2") 'omoikane-lisp/src/types:omoikane-parse-error))
  (ok (signals (omoikane-read ")") 'omoikane-lisp/src/types:omoikane-parse-error)))

(deftest test-read-reject-package-prefix
  (ok (signals (omoikane-read "cl:car") 'omoikane-lisp/src/types:omoikane-parse-error)))

(deftest test-read-reject-reader-macro
  (ok (signals (omoikane-read "#.42") 'omoikane-lisp/src/types:omoikane-parse-error))
  (ok (signals (omoikane-read "#'car") 'omoikane-lisp/src/types:omoikane-parse-error)))
```

**Step 2: Run tests to verify they fail**

```
load-system: omoikane-lisp
run-tests: omoikane-lisp/tests
```
Expected: FAIL (functions not defined)

**Step 3: Implement reader**

`src/reader.lisp` — a hand-written recursive descent reader:

```lisp
(defpackage :omoikane-lisp/src/reader
  (:use :cl :omoikane-lisp/src/types)
  (:export #:omoikane-read #:omoikane-read-program))
(in-package :omoikane-lisp/src/reader)

(defun omoikane-read (input)
  "Read a single expression from INPUT string."
  (multiple-value-bind (expr pos) (read-expr input 0)
    (let ((pos (skip-whitespace-and-comments input pos)))
      (declare (ignore pos))
      expr)))

(defun omoikane-read-program (input)
  "Read all top-level expressions from INPUT string. Returns a list."
  (let ((exprs '())
        (pos 0)
        (len (length input)))
    (loop
      (setf pos (skip-whitespace-and-comments input pos))
      (when (>= pos len) (return (nreverse exprs)))
      (multiple-value-bind (expr new-pos) (read-expr input pos)
        (push expr exprs)
        (setf pos new-pos)))))

;;; --- Internal parser ---

(defun read-expr (input pos)
  "Parse one expression from INPUT starting at POS. Returns (values expr new-pos)."
  (setf pos (skip-whitespace-and-comments input pos))
  (when (>= pos (length input))
    (error 'omoikane-parse-error :message "Unexpected end of input"))
  (let ((ch (char input pos)))
    (cond
      ((char= ch #\() (read-list input (1+ pos)))
      ((char= ch #\)) (error 'omoikane-parse-error
                              :message "Unexpected closing parenthesis"))
      ((char= ch #\') (read-quote input (1+ pos)))
      ((char= ch #\#) (error 'omoikane-parse-error
                              :message "Reader macros (#) are not allowed"))
      (t (read-atom input pos)))))

(defun read-list (input pos)
  "Parse a list body after opening '('. Returns (values list new-pos)."
  (let ((elements '()))
    (loop
      (setf pos (skip-whitespace-and-comments input pos))
      (when (>= pos (length input))
        (error 'omoikane-parse-error :message "Unterminated list — missing ')'"))
      (when (char= (char input pos) #\))
        (return (values (nreverse elements) (1+ pos))))
      (multiple-value-bind (expr new-pos) (read-expr input pos)
        (push expr elements)
        (setf pos new-pos)))))

(defun read-quote (input pos)
  "Parse a quoted expression after '."
  (multiple-value-bind (expr new-pos) (read-expr input pos)
    (values (list "quote" expr) new-pos)))

(defun read-atom (input pos)
  "Parse an atom (integer or symbol) starting at POS."
  (let ((start pos)
        (len (length input)))
    (loop while (and (< pos len) (atom-char-p (char input pos)))
          do (incf pos))
    (when (= start pos)
      (error 'omoikane-parse-error
             :message (format nil "Unexpected character: ~a" (char input pos))))
    (let ((token (subseq input start pos)))
      ;; Reject package prefixes
      (when (find #\: token)
        (error 'omoikane-parse-error
               :message (format nil "Package prefix not allowed: ~a" token)))
      (values (parse-token token) pos))))

(defun parse-token (token)
  "Convert a token string to an AST value."
  (cond
    ((string= token "t") t)
    ((string= token "nil") nil)
    ((integer-token-p token) (parse-integer token 10))
    (t (string-downcase token))))

(defun integer-token-p (token)
  "Check if TOKEN looks like an integer."
  (let ((start (if (and (> (length token) 1)
                        (or (char= (char token 0) #\-)
                            (char= (char token 0) #\+)))
                   1
                   0)))
    (and (> (length token) start)
         (every #'digit-char-p (subseq token start)))))

(defun atom-char-p (ch)
  "Is CH a valid atom character?"
  (and (not (char= ch #\())
       (not (char= ch #\)))
       (not (char= ch #\'))
       (not (char= ch #\;))
       (not (char= ch #\#))
       (not (whitespace-p ch))))

(defun whitespace-p (ch)
  (or (char= ch #\Space)
      (char= ch #\Tab)
      (char= ch #\Newline)
      (char= ch #\Return)))

(defun skip-whitespace-and-comments (input pos)
  "Skip whitespace and ;-comments."
  (let ((len (length input)))
    (loop
      (when (>= pos len) (return pos))
      (cond
        ((whitespace-p (char input pos)) (incf pos))
        ((char= (char input pos) #\;)
         (loop while (and (< pos len) (char/= (char input pos) #\Newline))
               do (incf pos)))
        (t (return pos))))))
```

**Step 4: Run tests to verify they pass**

```
load-system: omoikane-lisp (force reload)
run-tests: omoikane-lisp/tests
```
Expected: All reader-test tests PASS.

**Step 5: Commit**

```bash
git add src/reader.lisp tests/reader-test.lisp
git commit -m "feat: add safe S-expression reader with quote, comments, error handling"
```

---

### Task 3: Lexical Environment

**Files:**
- Modify: `src/env.lisp`
- Create: `tests/env-test.lisp`

**Step 1: Write environment tests**

```lisp
(defpackage :omoikane-lisp/tests/env-test
  (:use :cl :rove :omoikane-lisp/src/env :omoikane-lisp/src/types))
(in-package :omoikane-lisp/tests/env-test)

(deftest test-env-lookup
  (let ((env (env-extend nil '("x") '(42))))
    (ok (= 42 (env-lookup env "x")))))

(deftest test-env-lookup-missing
  (ok (signals (env-lookup nil "x") 'omoikane-name-error)))

(deftest test-env-nested
  (let* ((outer (env-extend nil '("x") '(1)))
         (inner (env-extend outer '("y") '(2))))
    (ok (= 1 (env-lookup inner "x")))
    (ok (= 2 (env-lookup inner "y")))))

(deftest test-env-shadow
  (let* ((outer (env-extend nil '("x") '(1)))
         (inner (env-extend outer '("x") '(99))))
    (ok (= 99 (env-lookup inner "x")))))

(deftest test-env-set
  (let ((env (env-extend nil '("x") '(1))))
    (env-set! env "x" 42)
    (ok (= 42 (env-lookup env "x")))))
```

**Step 2: Implement environment**

```lisp
(defpackage :omoikane-lisp/src/env
  (:use :cl :omoikane-lisp/src/types)
  (:export #:env-extend #:env-lookup #:env-set!))
(in-package :omoikane-lisp/src/env)

(defun env-extend (parent names values)
  "Create a new environment frame extending PARENT with NAMES bound to VALUES."
  (let ((frame (mapcar #'cons names values)))
    (cons frame parent)))

(defun env-lookup (env name)
  "Look up NAME in ENV. Signals omoikane-name-error if not found."
  (loop for frame in env
        do (let ((pair (assoc name frame :test #'string=)))
             (when pair (return-from env-lookup (cdr pair)))))
  (error 'omoikane-name-error
         :message (format nil "Undefined variable: ~a" name)))

(defun env-set! (env name value)
  "Set NAME to VALUE in the first frame where it's found."
  (loop for frame in env
        do (let ((pair (assoc name frame :test #'string=)))
             (when pair
               (setf (cdr pair) value)
               (return-from env-set! value))))
  (error 'omoikane-name-error
         :message (format nil "Cannot set undefined variable: ~a" name)))
```

**Step 3: Run tests**

Expected: All env-test tests PASS.

**Step 4: Commit**

```bash
git add src/env.lisp tests/env-test.lisp
git commit -m "feat: add lexical environment with extend, lookup, set"
```

---

### Task 4: Core Evaluator

**Files:**
- Modify: `src/evaluator.lisp`
- Create: `tests/evaluator-test.lisp`

The evaluator is the heart of the system. It takes an AST expression, an environment, and an execution context, and returns a value.

**Step 1: Write evaluator tests for literals and special forms**

```lisp
(defpackage :omoikane-lisp/tests/evaluator-test
  (:use :cl :rove
        :omoikane-lisp/src/types
        :omoikane-lisp/src/evaluator))
(in-package :omoikane-lisp/tests/evaluator-test)

(defun eval1 (code &key (fuel 1000))
  "Helper: parse and evaluate a single expression."
  (eval-string code :fuel fuel))

;; --- Literals ---
(deftest test-eval-integer
  (ok (= 42 (eval1 "42"))))

(deftest test-eval-boolean
  (ok (eq t (eval1 "t")))
  (ok (eq nil (eval1 "nil"))))

;; --- Quote ---
(deftest test-eval-quote
  (ok (equal "x" (eval1 "'x")))
  ;; Quoted list returns an ocons chain
  (let ((result (eval1 "'(1 2 3)")))
    (ok (ocons-p result))
    (ok (= 1 (ocons-ocar result)))))

;; --- If ---
(deftest test-eval-if
  (ok (= 1 (eval1 "(if t 1 2)")))
  (ok (= 2 (eval1 "(if nil 1 2)")))
  ;; Only false branch not evaluated
  (ok (= 3 (eval1 "(if t 3)")))
  (ok (eq nil (eval1 "(if nil 3)"))))

;; --- Let ---
(deftest test-eval-let
  (ok (= 42 (eval1 "(let ((x 42)) x)")))
  (ok (= 3 (eval1 "(let ((x 1) (y 2)) (+ x y))"))))

;; --- Lambda & application ---
(deftest test-eval-lambda
  (ok (= 3 (eval1 "((lambda (x) (+ x 1)) 2)"))))

;; --- Define ---
(deftest test-eval-define
  (ok (= 6 (eval1 "(define (double x) (+ x x)) (double 3)")))
  (ok (= 120 (eval1 "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 5)"))))

;; --- Cond ---
(deftest test-eval-cond
  (ok (= 2 (eval1 "(cond (nil 1) (t 2))")))
  (ok (eq nil (eval1 "(cond (nil 1))"))))

;; --- And / Or ---
(deftest test-eval-and
  (ok (= 2 (eval1 "(and 1 2)")))
  (ok (eq nil (eval1 "(and nil 2)")))
  (ok (eq t (eval1 "(and)"))))

(deftest test-eval-or
  (ok (= 1 (eval1 "(or 1 2)")))
  (ok (= 2 (eval1 "(or nil 2)")))
  (ok (eq nil (eval1 "(or)"))))

;; --- Step counting ---
(deftest test-eval-step-counting
  (ok (signals (eval1 "(define (f x) (f x)) (f 1)" :fuel 100)
               'omoikane-step-limit-exceeded)))
```

**Step 2: Implement evaluator**

```lisp
(defpackage :omoikane-lisp/src/evaluator
  (:use :cl
        :omoikane-lisp/src/types
        :omoikane-lisp/src/reader
        :omoikane-lisp/src/env
        :omoikane-lisp/src/builtins)
  (:export #:omoikane-eval #:eval-string))
(in-package :omoikane-lisp/src/evaluator)

(defun eval-string (input &key (fuel 10000) (max-depth 100)
                               (max-cons 10000) (max-output 1000)
                               (max-integer (expt 2 64)))
  "Parse and evaluate INPUT string. Returns the result of the last expression."
  (let ((program (omoikane-read-program input))
        (ctx (make-exec-ctx :fuel fuel :max-depth max-depth
                            :max-cons max-cons :max-output max-output
                            :max-integer max-integer))
        (env (make-initial-env)))
    (let ((result nil))
      (dolist (expr program result)
        (setf result (omoikane-eval expr env ctx))))))

(defun omoikane-eval (expr env ctx)
  "Evaluate EXPR in ENV with execution context CTX."
  (consume-fuel ctx)
  (cond
    ;; Self-evaluating: integers, booleans
    ((integerp expr) (check-integer ctx expr))
    ((eq expr t) t)
    ((null expr) nil)
    ;; Symbol lookup
    ((stringp expr) (env-lookup env expr))
    ;; Compound forms
    ((consp expr) (eval-compound expr env ctx))
    (t (error 'omoikane-internal-error
              :message (format nil "Unknown expression type: ~s" expr)))))

(defun eval-compound (expr env ctx)
  "Evaluate a compound (list) expression."
  (let ((head (car expr))
        (args (cdr expr)))
    (cond
      ((string= head "quote")  (eval-quote args ctx))
      ((string= head "if")     (eval-if args env ctx))
      ((string= head "let")    (eval-let args env ctx))
      ((string= head "let*")   (eval-let* args env ctx))
      ((string= head "lambda") (eval-lambda args env))
      ((string= head "define") (eval-define args env ctx))
      ((string= head "cond")   (eval-cond args env ctx))
      ((string= head "and")    (eval-and args env ctx))
      ((string= head "or")     (eval-or args env ctx))
      (t (eval-application head args env ctx)))))

;;; --- Special forms ---

(defun eval-quote (args ctx)
  (when (/= (length args) 1)
    (error 'omoikane-arity-error :message "quote requires exactly 1 argument"))
  (ast-to-value (car args) ctx))

(defun ast-to-value (ast ctx)
  "Convert a quoted AST to a runtime value (ocons chain for lists)."
  (cond
    ((null ast) nil)
    ((integerp ast) (check-integer ctx ast))
    ((eq ast t) t)
    ((stringp ast) ast)  ; quoted symbols remain as strings
    ((consp ast)
     (track-cons ctx)
     (make-ocons (ast-to-value (car ast) ctx)
                 (ast-to-value (cdr ast) ctx)))
    (t ast)))

(defun eval-if (args env ctx)
  (let ((len (length args)))
    (when (or (< len 2) (> len 3))
      (error 'omoikane-arity-error :message "if requires 2 or 3 arguments"))
    (if (omoikane-eval (first args) env ctx)
        (omoikane-eval (second args) env ctx)
        (if (= len 3)
            (omoikane-eval (third args) env ctx)
            nil))))

(defun eval-let (args env ctx)
  (when (< (length args) 2)
    (error 'omoikane-arity-error :message "let requires bindings and body"))
  (let* ((bindings (first args))
         (body (rest args))
         (names (mapcar #'first bindings))
         (values (mapcar (lambda (b) (omoikane-eval (second b) env ctx))
                         bindings))
         (new-env (env-extend env names values)))
    (let ((result nil))
      (dolist (expr body result)
        (setf result (omoikane-eval expr new-env ctx))))))

(defun eval-let* (args env ctx)
  (when (< (length args) 2)
    (error 'omoikane-arity-error :message "let* requires bindings and body"))
  (let ((bindings (first args))
        (body (rest args))
        (current-env env))
    (dolist (binding bindings)
      (let ((val (omoikane-eval (second binding) current-env ctx)))
        (setf current-env (env-extend current-env
                                      (list (first binding))
                                      (list val)))))
    (let ((result nil))
      (dolist (expr body result)
        (setf result (omoikane-eval expr current-env ctx))))))

(defun eval-lambda (args env)
  (when (< (length args) 2)
    (error 'omoikane-arity-error :message "lambda requires params and body"))
  (let ((params (first args))
        (body (if (= 1 (length (rest args)))
                  (second args)
                  (cons "begin" (rest args)))))
    (make-closure params body env)))

(defun eval-define (args env ctx)
  "Handle (define name value) and (define (name params...) body...)."
  (let ((target (first args)))
    (cond
      ;; (define (f x y) body...) → function definition
      ((consp target)
       (let* ((name (first target))
              (params (rest target))
              (body (if (= 1 (length (rest args)))
                        (second args)
                        (cons "begin" (rest args))))
              (closure (make-closure params body env name)))
         ;; Self-reference: put closure in its own env
         (let ((new-env (env-extend env (list name) (list closure))))
           (setf (closure-env closure) new-env)
           ;; Also add to calling env for subsequent defines
           (let ((frame (list (cons name closure))))
             (nconc env (list frame)))
           closure)))
      ;; (define name value)
      ((stringp target)
       (let ((value (omoikane-eval (second args) env ctx)))
         (let ((frame (list (cons target value))))
           (nconc env (list frame)))
         value))
      (t (error 'omoikane-parse-error
                :message (format nil "Invalid define target: ~s" target))))))

(defun eval-cond (clauses env ctx)
  (dolist (clause clauses nil)
    (let ((test-result (omoikane-eval (first clause) env ctx)))
      (when test-result
        (return (if (rest clause)
                    (omoikane-eval (second clause) env ctx)
                    test-result))))))

(defun eval-and (args env ctx)
  (if (null args)
      t
      (let ((result nil))
        (dolist (arg args result)
          (setf result (omoikane-eval arg env ctx))
          (unless result (return nil))))))

(defun eval-or (args env ctx)
  (if (null args)
      nil
      (dolist (arg args nil)
        (let ((result (omoikane-eval arg env ctx)))
          (when result (return result))))))

;;; --- Function application ---

(defun eval-application (operator args env ctx)
  "Evaluate a function call."
  (let ((func (omoikane-eval operator env ctx))
        (evaluated-args (mapcar (lambda (a) (omoikane-eval a env ctx)) args)))
    (apply-function func evaluated-args ctx)))

(defun apply-function (func args ctx)
  "Apply FUNC to ARGS."
  (consume-fuel ctx 4)  ; function call overhead
  (cond
    ((closure-p func)
     (let ((params (closure-params func)))
       (when (/= (length params) (length args))
         (error 'omoikane-arity-error
                :message (format nil "~a expects ~d args, got ~d"
                                 (or (closure-name func) "lambda")
                                 (length params) (length args))))
       (track-depth ctx 1)
       (unwind-protect
            (let ((call-env (env-extend (closure-env func) params args)))
              (omoikane-eval (closure-body func) call-env ctx))
         (track-depth ctx -1))))
    ((builtin-p func)
     (when (and (builtin-arity func)
                (/= (builtin-arity func) (length args)))
       (error 'omoikane-arity-error
              :message (format nil "~a expects ~d args, got ~d"
                               (builtin-name func)
                               (builtin-arity func) (length args))))
     (funcall (builtin-func func) args ctx))
    (t (error 'omoikane-type-error
              :message (format nil "Not a function: ~s" func)))))
```

**Step 3: Run tests**

Expected: evaluator-test tests PASS (requires builtins for `+`, `-`, `*`, `=` — implement those in Task 5 first, or add a minimal set inline for testing).

> **Note:** Tasks 4 and 5 are somewhat coupled. Implement builtins stub (Task 5 Step 1) before running evaluator tests that use builtins like `+`.

**Step 4: Commit**

```bash
git add src/evaluator.lisp tests/evaluator-test.lisp
git commit -m "feat: add core evaluator with special forms and function application"
```

---

### Task 5: Built-in Functions

**Files:**
- Modify: `src/builtins.lisp`
- Add builtin tests to `tests/evaluator-test.lisp`

**Step 1: Write builtin tests (append to evaluator-test.lisp)**

```lisp
;; --- Arithmetic ---
(deftest test-builtin-add
  (ok (= 3 (eval1 "(+ 1 2)")))
  (ok (= 6 (eval1 "(+ 1 2 3)")))
  (ok (= 0 (eval1 "(+)"))))

(deftest test-builtin-sub
  (ok (= 1 (eval1 "(- 3 2)")))
  (ok (= -5 (eval1 "(- 5)"))))

(deftest test-builtin-mul
  (ok (= 6 (eval1 "(* 2 3)")))
  (ok (= 1 (eval1 "(*)"))))

(deftest test-builtin-div
  (ok (= 3 (eval1 "(div 7 2)"))))

(deftest test-builtin-mod
  (ok (= 1 (eval1 "(mod 7 2)"))))

;; --- Comparison ---
(deftest test-builtin-eq
  (ok (eq t (eval1 "(= 1 1)")))
  (ok (eq nil (eval1 "(= 1 2)"))))

(deftest test-builtin-lt
  (ok (eq t (eval1 "(< 1 2)")))
  (ok (eq nil (eval1 "(< 2 1)"))))

;; --- List operations ---
(deftest test-builtin-cons
  (let ((result (eval1 "(cons 1 2)")))
    (ok (ocons-p result))
    (ok (= 1 (ocons-ocar result)))
    (ok (= 2 (ocons-ocdr result)))))

(deftest test-builtin-car-cdr
  (ok (= 1 (eval1 "(car (cons 1 2))")))
  (ok (= 2 (eval1 "(cdr (cons 1 2))"))))

(deftest test-builtin-list
  (let ((result (eval1 "(list 1 2 3)")))
    (ok (ocons-p result))
    (ok (= 1 (ocons-ocar result)))))

(deftest test-builtin-null?
  (ok (eq t (eval1 "(null? nil)")))
  (ok (eq nil (eval1 "(null? 1)"))))

(deftest test-builtin-atom?
  (ok (eq t (eval1 "(atom? 1)")))
  (ok (eq nil (eval1 "(atom? (cons 1 2))"))))

(deftest test-builtin-length
  (ok (= 3 (eval1 "(length (list 1 2 3))")))
  (ok (= 0 (eval1 "(length nil)"))))

;; --- Other ---
(deftest test-builtin-not
  (ok (eq t (eval1 "(not nil)")))
  (ok (eq nil (eval1 "(not t)"))))

(deftest test-builtin-eq?
  (ok (eq t (eval1 "(eq? 1 1)")))
  (ok (eq nil (eval1 "(eq? 1 2)"))))
```

**Step 2: Implement builtins**

```lisp
(defpackage :omoikane-lisp/src/builtins
  (:use :cl :omoikane-lisp/src/types :omoikane-lisp/src/env)
  (:export #:make-initial-env))
(in-package :omoikane-lisp/src/builtins)

(defun make-initial-env ()
  "Create the initial environment with all built-in functions."
  (let ((builtins
          (list
           ;; Arithmetic
           (cons "+" (make-builtin "+" #'builtin-add nil))
           (cons "-" (make-builtin "-" #'builtin-sub nil))
           (cons "*" (make-builtin "*" #'builtin-mul nil))
           (cons "div" (make-builtin "div" #'builtin-div 2))
           (cons "mod" (make-builtin "mod" #'builtin-mod 2))
           ;; Comparison
           (cons "=" (make-builtin "=" #'builtin-eq 2))
           (cons "<" (make-builtin "<" #'builtin-lt 2))
           (cons "<=" (make-builtin "<=" #'builtin-le 2))
           (cons ">" (make-builtin ">" #'builtin-gt 2))
           (cons ">=" (make-builtin ">=" #'builtin-ge 2))
           ;; List
           (cons "cons" (make-builtin "cons" #'builtin-cons 2))
           (cons "car" (make-builtin "car" #'builtin-car 1))
           (cons "cdr" (make-builtin "cdr" #'builtin-cdr 1))
           (cons "list" (make-builtin "list" #'builtin-list nil))
           (cons "null?" (make-builtin "null?" #'builtin-null? 1))
           (cons "atom?" (make-builtin "atom?" #'builtin-atom? 1))
           (cons "length" (make-builtin "length" #'builtin-length 1))
           ;; Other
           (cons "not" (make-builtin "not" #'builtin-not 1))
           (cons "eq?" (make-builtin "eq?" #'builtin-eq? 2))
           (cons "print" (make-builtin "print" #'builtin-print 1)))))
    (list builtins)))

;;; --- Helpers ---

(defun ensure-integer (name value)
  (unless (integerp value)
    (error 'omoikane-type-error
           :message (format nil "~a: expected integer, got ~s" name value))))

(defun ensure-ocons (name value)
  (unless (ocons-p value)
    (error 'omoikane-type-error
           :message (format nil "~a: expected list/pair, got ~s" name value))))

;;; --- Arithmetic ---

(defun builtin-add (args ctx)
  (let ((result (reduce #'+ args :initial-value 0)))
    (dolist (a args) (ensure-integer "+" a))
    (check-integer ctx result)))

(defun builtin-sub (args ctx)
  (when (null args)
    (error 'omoikane-arity-error :message "- requires at least 1 argument"))
  (dolist (a args) (ensure-integer "-" a))
  (let ((result (if (= 1 (length args))
                    (- (first args))
                    (reduce #'- args))))
    (check-integer ctx result)))

(defun builtin-mul (args ctx)
  (dolist (a args) (ensure-integer "*" a))
  (let ((result (reduce #'* args :initial-value 1)))
    (check-integer ctx result)))

(defun builtin-div (args ctx)
  (dolist (a args) (ensure-integer "div" a))
  (when (zerop (second args))
    (error 'omoikane-type-error :message "div: division by zero"))
  (check-integer ctx (truncate (first args) (second args))))

(defun builtin-mod (args ctx)
  (declare (ignore ctx))
  (dolist (a args) (ensure-integer "mod" a))
  (when (zerop (second args))
    (error 'omoikane-type-error :message "mod: division by zero"))
  (cl:mod (first args) (second args)))

;;; --- Comparison ---

(defun builtin-eq (args ctx)
  (declare (ignore ctx))
  (dolist (a args) (ensure-integer "=" a))
  (if (cl:= (first args) (second args)) t nil))

(defun builtin-lt (args ctx)
  (declare (ignore ctx))
  (dolist (a args) (ensure-integer "<" a))
  (if (cl:< (first args) (second args)) t nil))

(defun builtin-le (args ctx)
  (declare (ignore ctx))
  (dolist (a args) (ensure-integer "<=" a))
  (if (cl:<= (first args) (second args)) t nil))

(defun builtin-gt (args ctx)
  (declare (ignore ctx))
  (dolist (a args) (ensure-integer ">" a))
  (if (cl:> (first args) (second args)) t nil))

(defun builtin-ge (args ctx)
  (declare (ignore ctx))
  (dolist (a args) (ensure-integer ">=" a))
  (if (cl:>= (first args) (second args)) t nil))

;;; --- List operations ---

(defun builtin-cons (args ctx)
  (track-cons ctx)
  (make-ocons (first args) (second args)))

(defun builtin-car (args ctx)
  (declare (ignore ctx))
  (let ((pair (first args)))
    (if (null pair)
        nil
        (progn (ensure-ocons "car" pair) (ocons-ocar pair)))))

(defun builtin-cdr (args ctx)
  (declare (ignore ctx))
  (let ((pair (first args)))
    (if (null pair)
        nil
        (progn (ensure-ocons "cdr" pair) (ocons-ocdr pair)))))

(defun builtin-list (args ctx)
  (let ((result nil))
    (dolist (a (reverse args) result)
      (track-cons ctx)
      (setf result (make-ocons a result)))))

(defun builtin-null? (args ctx)
  (declare (ignore ctx))
  (if (null (first args)) t nil))

(defun builtin-atom? (args ctx)
  (declare (ignore ctx))
  (if (ocons-p (first args)) nil t))

(defun builtin-length (args ctx)
  (declare (ignore ctx))
  (let ((lst (first args))
        (count 0))
    (loop while (ocons-p lst)
          do (incf count) (setf lst (ocons-ocdr lst)))
    count))

;;; --- Other ---

(defun builtin-not (args ctx)
  (declare (ignore ctx))
  (if (first args) nil t))

(defun builtin-eq? (args ctx)
  (declare (ignore ctx))
  (let ((a (first args))
        (b (second args)))
    (if (cond
          ((and (integerp a) (integerp b)) (cl:= a b))
          ((and (stringp a) (stringp b)) (string= a b))
          (t (eql a b)))
        t nil)))

(defun builtin-print (args ctx)
  (let ((value (first args))
        (output (exec-ctx-output ctx)))
    (let ((str (print-value value)))
      (when (> (+ (length output) (length str)) (exec-ctx-max-output ctx))
        (error 'omoikane-output-limit-exceeded
               :message "Output limit exceeded"))
      (loop for ch across str do (vector-push-extend ch output))
      (vector-push-extend #\Newline output))
    value))

(defun print-value (value)
  "Convert a runtime value to its string representation."
  (cond
    ((null value) "nil")
    ((eq value t) "t")
    ((integerp value) (format nil "~d" value))
    ((stringp value) value)  ; symbols print as their name
    ((ocons-p value) (print-ocons value))
    ((closure-p value) (format nil "#<closure~@[ ~a~]>" (closure-name value)))
    ((builtin-p value) (format nil "#<builtin ~a>" (builtin-name value)))
    (t (format nil "#<unknown>"))))

(defun print-ocons (cell)
  "Print an ocons chain as a list or dotted pair."
  (with-output-to-string (s)
    (write-char #\( s)
    (write-string (print-value (ocons-ocar cell)) s)
    (let ((rest (ocons-ocdr cell)))
      (loop while (ocons-p rest)
            do (write-char #\Space s)
               (write-string (print-value (ocons-ocar rest)) s)
               (setf rest (ocons-ocdr rest)))
      (unless (null rest)
        (write-string " . " s)
        (write-string (print-value rest) s)))
    (write-char #\) s)))
```

**Step 3: Run all tests**

Expected: All evaluator and builtin tests PASS.

**Step 4: Commit**

```bash
git add src/builtins.lisp
git commit -m "feat: add built-in functions (arithmetic, comparison, list, print)"
```

---

### Task 6: Safety Controls & Limits

**Files:**
- Create: `tests/safety-test.lisp`

Safety is already wired into the evaluator via `exec-ctx`. This task adds comprehensive tests for all limit types.

**Step 1: Write safety tests**

```lisp
(defpackage :omoikane-lisp/tests/safety-test
  (:use :cl :rove
        :omoikane-lisp/src/types
        :omoikane-lisp/src/evaluator))
(in-package :omoikane-lisp/tests/safety-test)

(defun eval-safe (code &key (fuel 100) (max-depth 10) (max-cons 50)
                            (max-output 100) (max-integer 1000000))
  (eval-string code :fuel fuel :max-depth max-depth :max-cons max-cons
                    :max-output max-output :max-integer max-integer))

;; --- Step limit ---
(deftest test-infinite-recursion-stops
  (ok (signals
       (eval-safe "(define (f x) (f x)) (f 1)")
       'omoikane-step-limit-exceeded)))

(deftest test-deep-computation-stops
  (ok (signals
       (eval-safe "(define (f n) (if (= n 0) 0 (+ 1 (f (- n 1))))) (f 10000)"
                  :fuel 50)
       'omoikane-step-limit-exceeded)))

;; --- Recursion depth ---
(deftest test-deep-recursion-stops
  (ok (signals
       (eval-safe "(define (f n) (if (= n 0) 0 (+ 1 (f (- n 1))))) (f 100)"
                  :fuel 100000 :max-depth 5)
       'omoikane-recursion-limit-exceeded)))

;; --- Memory / cons limit ---
(deftest test-huge-list-stops
  (ok (signals
       (eval-safe "(define (make n) (if (= n 0) nil (cons n (make (- n 1))))) (make 1000)"
                  :fuel 100000 :max-cons 10)
       'omoikane-memory-limit-exceeded)))

;; --- Integer size limit ---
(deftest test-huge-integer-stops
  (ok (signals
       (eval-safe "(* 999999 999999)" :max-integer 1000000)
       'omoikane-integer-limit-exceeded)))

;; --- Output limit ---
(deftest test-output-limit
  (ok (signals
       (eval-safe "(define (spam n) (if (= n 0) nil (let () (print n) (spam (- n 1))))) (spam 100)"
                  :fuel 100000 :max-output 20)
       'omoikane-output-limit-exceeded)))

;; --- Name error ---
(deftest test-undefined-variable
  (ok (signals (eval-safe "undefined-var") 'omoikane-name-error)))

;; --- Type error ---
(deftest test-type-error-arithmetic
  (ok (signals (eval-safe "(+ 1 t)") 'omoikane-type-error)))

(deftest test-type-error-car
  (ok (signals (eval-safe "(car 42)") 'omoikane-type-error)))

;; --- Arity error ---
(deftest test-arity-error
  (ok (signals (eval-safe "((lambda (x) x) 1 2)") 'omoikane-arity-error)))

;; --- No host eval ---
(deftest test-no-eval-access
  (ok (signals (eval-safe "(eval '(+ 1 2))") 'omoikane-name-error)))

;; --- No setf/setq ---
(deftest test-no-mutation
  ;; setf and setq are not special forms, so they'd be looked up as functions
  (ok (signals (eval-safe "(setf x 1)") 'omoikane-name-error))
  (ok (signals (eval-safe "(setq x 1)") 'omoikane-name-error)))
```

**Step 2: Run safety tests**

Expected: All safety tests PASS.

**Step 3: Commit**

```bash
git add tests/safety-test.lisp
git commit -m "test: add comprehensive safety tests for all resource limits"
```

---

### Task 7: Public API & Integration Tests

**Files:**
- Modify: `src/main.lisp`
- Create: `tests/integration-test.lisp`

**Step 1: Implement public API**

The public API wraps `eval-string` and returns structured results:

```lisp
(defpackage :omoikane-lisp
  (:use :cl
        :omoikane-lisp/src/types
        :omoikane-lisp/src/evaluator
        :omoikane-lisp/src/builtins)
  (:export #:evaluate #:print-value
           ;; Re-export conditions for handler-case
           #:omoikane-error #:omoikane-error-message
           #:omoikane-parse-error #:omoikane-name-error
           #:omoikane-type-error #:omoikane-arity-error
           #:omoikane-step-limit-exceeded
           #:omoikane-recursion-limit-exceeded
           #:omoikane-memory-limit-exceeded
           #:omoikane-integer-limit-exceeded
           #:omoikane-output-limit-exceeded
           #:omoikane-timeout-exceeded))
(in-package :omoikane-lisp)

(defun evaluate (code &key (fuel 10000) (max-depth 100)
                           (max-cons 10000) (max-output 1000)
                           (max-integer (expt 2 64))
                           (timeout 5))
  "Evaluate CODE string in the restricted Lisp.
Returns (values result metrics-plist).
On error, returns (values nil metrics-plist) with :error-type and :error-message in metrics."
  (let ((ctx (make-exec-ctx :fuel fuel :max-depth max-depth
                            :max-cons max-cons :max-output max-output
                            :max-integer max-integer)))
    (handler-case
        (let* ((result (with-timeout timeout
                         (let ((program (omoikane-lisp/src/reader:omoikane-read-program code))
                               (env (make-initial-env)))
                           (let ((r nil))
                             (dolist (expr program r)
                               (setf r (omoikane-eval expr env ctx)))))))
               (metrics (make-metrics ctx)))
          (values result metrics))
      (omoikane-error (e)
        (values nil (make-metrics ctx
                                  :error-type (type-of e)
                                  :error-message (omoikane-error-message e)))))))

(defun make-metrics (ctx &key error-type error-message)
  (list :steps-used (exec-ctx-steps-used ctx)
        :max-depth-reached (exec-ctx-max-depth-reached ctx)
        :cons-allocated (exec-ctx-cons-count ctx)
        :output (copy-seq (exec-ctx-output ctx))
        :fuel-remaining (exec-ctx-fuel ctx)
        :error-type error-type
        :error-message error-message))

(defun with-timeout (seconds thunk)
  "Execute THUNK with a wall-clock timeout. Simple trivial_timeout version."
  (declare (ignore seconds))
  ;; Initial version: rely on fuel for stopping.
  ;; Full timeout with sb-ext:with-timeout can be added for production.
  (funcall thunk))
```

Note: `with-timeout` is a placeholder. For production, use `sb-ext:with-timeout` (SBCL-specific). The fuel system already prevents runaway computation.

**Step 2: Write integration tests (acceptance criteria from PRD Section 19)**

```lisp
(defpackage :omoikane-lisp/tests/integration-test
  (:use :cl :rove :omoikane-lisp :omoikane-lisp/src/types))
(in-package :omoikane-lisp/tests/integration-test)

;;; PRD Acceptance Criterion 1:
;;; "ユーザ定義関数と再帰を含む小問題を解ける"
(deftest test-factorial
  (multiple-value-bind (result metrics) (evaluate "
    (define (factorial n)
      (if (= n 0) 1 (* n (factorial (- n 1)))))
    (factorial 10)")
    (ok (= 3628800 result))
    (ok (> (getf metrics :steps-used) 0))))

(deftest test-fibonacci
  (multiple-value-bind (result metrics) (evaluate "
    (define (fib n)
      (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
    (fib 10)")
    (ok (= 55 result))
    (declare (ignore metrics))))

(deftest test-list-reverse
  (let ((result (evaluate "
    (define (rev lst acc)
      (if (null? lst) acc
          (rev (cdr lst) (cons (car lst) acc))))
    (rev (list 1 2 3 4 5) nil)")))
    ;; Result is ocons chain: (5 4 3 2 1)
    (ok (ocons-p result))
    (ok (= 5 (ocons-ocar result)))))

(deftest test-map-like
  (let ((result (evaluate "
    (define (my-map f lst)
      (if (null? lst) nil
          (cons (f (car lst)) (my-map f (cdr lst)))))
    (my-map (lambda (x) (* x x)) (list 1 2 3 4))")))
    (ok (ocons-p result))
    (ok (= 1 (ocons-ocar result)))
    (ok (= 4 (ocons-ocar (ocons-ocdr result))))))

;;; PRD Acceptance Criterion 2:
;;; "無限再帰コードが step または深さ制限で確実に停止する"
(deftest test-infinite-recursion-halts
  (multiple-value-bind (result metrics)
      (evaluate "(define (loop) (loop)) (loop)" :fuel 100)
    (ok (null result))
    (ok (eq 'omoikane-step-limit-exceeded (getf metrics :error-type)))))

;;; PRD Acceptance Criterion 3:
;;; "巨大リスト生成がメモリ制限で停止する"
(deftest test-huge-list-halts
  (multiple-value-bind (result metrics)
      (evaluate "
        (define (huge n)
          (if (= n 0) nil (cons n (huge (- n 1)))))
        (huge 10000)"
        :max-cons 100)
    (ok (null result))
    (ok (eq 'omoikane-memory-limit-exceeded (getf metrics :error-type)))))

;;; PRD Acceptance Criterion 4:
;;; "ホスト Lisp の eval を使っていない"
;;; → Verified by code inspection (no CL:EVAL in evaluator.lisp)

;;; PRD Acceptance Criterion 5:
;;; "ファイル、ネットワーク、OS 実行が不可能である"
(deftest test-no-file-access
  (multiple-value-bind (result metrics)
      (evaluate "(open \"foo.txt\")")
    (ok (null result))
    (ok (eq 'omoikane-name-error (getf metrics :error-type)))))

(deftest test-no-system-access
  (multiple-value-bind (result metrics)
      (evaluate "(run-program \"ls\")")
    (ok (null result))
    (ok (eq 'omoikane-name-error (getf metrics :error-type)))))

;;; PRD Acceptance Criterion 6:
;;; "実行ごとに step 数などのメタ情報が取得できる"
(deftest test-metrics-returned
  (multiple-value-bind (result metrics)
      (evaluate "(+ 1 2)")
    (ok (= 3 result))
    (ok (getf metrics :steps-used))
    (ok (integerp (getf metrics :steps-used)))
    (ok (integerp (getf metrics :max-depth-reached)))
    (ok (integerp (getf metrics :cons-allocated)))))

;;; Complete program: solving a real problem
(deftest test-sum-list
  (let ((result (evaluate "
    (define (sum lst)
      (if (null? lst) 0
          (+ (car lst) (sum (cdr lst)))))
    (sum (list 1 2 3 4 5))")))
    (ok (= 15 result))))

(deftest test-nested-defines
  (let ((result (evaluate "
    (define (square x) (* x x))
    (define (sum-of-squares a b) (+ (square a) (square b)))
    (sum-of-squares 3 4)")))
    (ok (= 25 result))))
```

**Step 3: Run all tests**

```
load-system: omoikane-lisp (force)
run-tests: omoikane-lisp/tests
```
Expected: ALL tests PASS.

**Step 4: Lint**

```bash
mallet src/*.lisp
```

**Step 5: Final compile check**

```lisp
(asdf:compile-system :omoikane-lisp :force t)
```

**Step 6: Commit**

```bash
git add -A
git commit -m "feat: add public API and integration tests for restricted Lisp evaluator"
```

---

## Summary of Acceptance Criteria Coverage

| # | Criterion | Where Tested |
|---|-----------|-------------|
| 1 | User functions + recursion | integration-test: factorial, fibonacci, reverse |
| 2 | Infinite recursion stops | safety-test + integration-test |
| 3 | Huge list stops | safety-test + integration-test |
| 4 | No host eval | Code inspection + safety-test (eval undefined) |
| 5 | No file/network/OS | integration-test |
| 6 | Metrics returned | integration-test: test-metrics-returned |
| 7 | Process isolation | Future: OS sandbox (not in scope for evaluator) |
| 8 | Dangerous input tests pass | safety-test suite |

## Files Created/Modified

```
src/types.lisp          Core types, conditions, exec-ctx
src/reader.lisp         Safe S-expression reader
src/env.lisp            Lexical environment
src/builtins.lisp       Built-in functions registry
src/evaluator.lisp      Tree-walking evaluator
src/main.lisp           Public API (evaluate function)
tests/types-test.lisp   Type and condition tests
tests/reader-test.lisp  Reader tests
tests/env-test.lisp     Environment tests
tests/evaluator-test.lisp  Evaluator + builtin tests
tests/safety-test.lisp  Safety limit tests
tests/integration-test.lisp  End-to-end acceptance tests
omoikane-lisp.asd       Updated system definition
```
