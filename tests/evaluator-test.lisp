(defpackage :wardlisp/tests/evaluator-test
  (:use :cl :rove
        :wardlisp/src/types
        :wardlisp/src/evaluator))
(in-package :wardlisp/tests/evaluator-test)

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
  (let ((result (eval1 "'(1 2 3)")))
    (ok (ocons-p result))
    (ok (= 1 (ocons-ocar result)))))

;; --- If ---
(deftest test-eval-if
  (ok (= 1 (eval1 "(if t 1 2)")))
  (ok (= 2 (eval1 "(if nil 1 2)")))
  (ok (= 3 (eval1 "(if t 3)")))
  (ok (eq nil (eval1 "(if nil 3)"))))

;; --- Let ---

(deftest test-eval-let
  (ok (= 42 (eval1 "(let ((x 42)) x)")))
  (ok (= 3 (eval1 "(let ((x 1) (y 2)) (+ x y))")))
  ;; Parallel binding: earlier bindings NOT visible to later ones
  (ok (signals (eval1 "(let ((x 1) (y (+ x 1))) y)")
               'wardlisp/src/types:wardlisp-name-error))
  ;; let* has sequential bindings: earlier visible to later
  (ok (= 2 (eval1 "(let* ((x 1) (y (+ x 1))) y)"))))

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
               'wardlisp-step-limit-exceeded)))

;; --- Arithmetic builtins ---
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

(deftest test-builtin-quotient
  (ok (= 3 (eval1 "(quotient 7 2)")))
  (ok (= -3 (eval1 "(quotient -7 2)"))))

(deftest test-builtin-smart-div
  ;; Exact integer result
  (ok (= 2 (eval1 "(/ 6 3)")))
  (ok (integerp (eval1 "(/ 6 3)")))
  ;; Non-exact returns float
  (ok (typep (eval1 "(/ 7 2)") 'double-float))
  (ok (= 3.5d0 (eval1 "(/ 7 2)")))
  ;; Float args always return float
  (ok (typep (eval1 "(/ 6.0 3)") 'double-float)))

(deftest test-builtin-mod
  (ok (= 1 (eval1 "(mod 7 2)"))))

;; --- Comparison builtins ---
(deftest test-builtin-eq
  (ok (eq t (eval1 "(= 1 1)")))
  (ok (eq nil (eval1 "(= 1 2)"))))

(deftest test-builtin-lt
  (ok (eq t (eval1 "(< 1 2)")))
  (ok (eq nil (eval1 "(< 2 1)"))))

;; --- List builtins ---
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

(deftest test-builtin-null
  (ok (eq t (eval1 "(null? nil)")))
  (ok (eq nil (eval1 "(null? 1)"))))

(deftest test-builtin-atom
  (ok (eq t (eval1 "(atom? 1)")))
  (ok (eq nil (eval1 "(atom? (cons 1 2))"))))

(deftest test-builtin-length
  (ok (= 3 (eval1 "(length (list 1 2 3))")))
  (ok (= 0 (eval1 "(length nil)"))))

;; --- Other builtins ---
(deftest test-builtin-not
  (ok (eq t (eval1 "(not nil)")))
  (ok (eq nil (eval1 "(not t)"))))

(deftest test-builtin-eqp
  (ok (eq t (eval1 "(eq? 1 1)")))
  (ok (eq nil (eval1 "(eq? 1 2)"))))

;;; --- Coverage: quote arity error ---
(deftest test-eval-quote-arity
  (ok (signals (eval1 "(quote)") 'wardlisp-arity-error))
  (ok (signals (eval1 "(quote a b)") 'wardlisp-arity-error)))

;;; --- Coverage: quote with boolean literals ---
(deftest test-eval-quote-boolean
  (ok (eq t (eval1 "'t")))
  (ok (eq nil (eval1 "'nil"))))

;;; --- Coverage: if arity error ---
(deftest test-eval-if-arity
  (ok (signals (eval1 "(if)") 'wardlisp-arity-error))
  (ok (signals (eval1 "(if t)") 'wardlisp-arity-error))
  (ok (signals (eval1 "(if t 1 2 3)") 'wardlisp-arity-error)))

;;; --- Coverage: let arity error ---
(deftest test-eval-let-arity
  (ok (signals (eval1 "(let ())") 'wardlisp-arity-error)))

;;; --- Coverage: lambda arity error and multi-body ---
(deftest test-eval-lambda-arity
  (ok (signals (eval1 "(lambda)") 'wardlisp-arity-error))
  (ok (signals (eval1 "(lambda (x))") 'wardlisp-arity-error)))

(deftest test-eval-lambda-multi-body
  (ok (= 3 (eval1 "((lambda (x) 1 2 (+ x 1)) 2)"))))

;;; --- Coverage: define variable form ---
(deftest test-eval-define-variable
  (ok (= 42 (eval1 "(define x 42) x")))
  (ok (= 15 (eval1 "(define x 10) (+ x 5)"))))

;;; --- Coverage: define multi-body function ---
(deftest test-eval-define-multi-body
  (ok (= 42 (eval1 "(define (f x) 1 2 x) (f 42)"))))

;;; --- Coverage: define invalid target ---
(deftest test-eval-define-invalid-target
  (ok (signals (eval1 "(define 42 10)") 'wardlisp-parse-error)))

;;; --- Coverage: cond body-less clause returning test value ---
(deftest test-eval-cond-bodyless
  (ok (= 42 (eval1 "(cond (42))")))
  (ok (eq t (eval1 "(cond (nil 1) (t))"))))

;;; --- Coverage: builtin arity error ---
(deftest test-builtin-arity-error
  (ok (signals (eval1 "(car 1 2)") 'wardlisp-arity-error))
  (ok (signals (eval1 "(cdr 1 2)") 'wardlisp-arity-error))
  (ok (signals (eval1 "(not 1 2)") 'wardlisp-arity-error))
  (ok (signals (eval1 "(null? 1 2)") 'wardlisp-arity-error)))

;;; --- Coverage: non-function call ---
(deftest test-non-function-call
  (ok (signals (eval1 "(42)") 'wardlisp-type-error))
  (ok (signals (eval1 "(t 1)") 'wardlisp-type-error)))

;;; --- Coverage: comparison builtins <=, >, >= ---
(deftest test-builtin-le
  (ok (eq t (eval1 "(<= 1 2)")))
  (ok (eq t (eval1 "(<= 2 2)")))
  (ok (eq nil (eval1 "(<= 3 2)"))))

(deftest test-builtin-gt
  (ok (eq t (eval1 "(> 2 1)")))
  (ok (eq nil (eval1 "(> 1 2)")))
  (ok (eq nil (eval1 "(> 2 2)"))))

(deftest test-builtin-ge
  (ok (eq t (eval1 "(>= 2 1)")))
  (ok (eq t (eval1 "(>= 2 2)")))
  (ok (eq nil (eval1 "(>= 1 2)"))))

;;; --- Coverage: sub zero args ---
(deftest test-builtin-sub-zero-args
  (ok (signals (eval1 "(-)") 'wardlisp-arity-error)))

;;; --- Coverage: quotient/div/mod zero division ---
(deftest test-builtin-quotient-zero
  (ok (signals (eval1 "(quotient 1 0)") 'wardlisp-type-error)))

(deftest test-builtin-fdiv-zero
  (ok (signals (eval1 "(/ 1 0)") 'wardlisp-type-error)))

(deftest test-builtin-mod-zero
  (ok (signals (eval1 "(mod 1 0)") 'wardlisp-type-error)))

;;; --- Coverage: car/cdr of nil ---
(deftest test-builtin-car-nil
  (ok (eq nil (eval1 "(car nil)"))))

(deftest test-builtin-cdr-nil
  (ok (eq nil (eval1 "(cdr nil)"))))

;;; --- Coverage: eq? with symbols and other types ---
(deftest test-builtin-eqp-symbols
  (ok (eq t (eval1 "(eq? 'a 'a)")))
  (ok (eq nil (eval1 "(eq? 'a 'b)")))
  (ok (eq t (eval1 "(eq? t t)")))
  (ok (eq nil (eval1 "(eq? t nil)")))
  (ok (eq nil (eval1 "(eq? 1 'a)"))))

;;; --- Coverage: print-value for various types ---
(deftest test-eval-print-various
  (ok (= 42 (eval1 "(print 42)")))
  (ok (eq t (eval1 "(print t)")))
  (ok (eq nil (eval1 "(print nil)")))
  ;; Print a list (exercises print-ocons)
  (let ((result (eval1 "(print (list 1 2 3))")))
    (ok (ocons-p result)))
  ;; Print a dotted pair (exercises dotted-pair branch)
  (let ((result (eval1 "(print (cons 1 2))")))
    (ok (ocons-p result)))
  ;; Print a symbol
  (ok (equal "hello" (eval1 "(print 'hello)")))
  ;; Print a lambda (exercises closure printing)
  (let ((result (eval1 "(print (lambda (x) x))")))
    (ok (closure-p result)))
  ;; Print a builtin (exercises builtin printing)
  (let ((result (eval1 "(print +)")))
    (ok (builtin-p result))))

;;; --- Coverage: computed operator (non-string head) ---
(deftest test-eval-computed-operator
  (ok (= 3 (eval1 "((if t + -) 1 2)"))))

;;; --- Coverage: begin special form ---
(deftest test-eval-begin
  (ok (= 3 (eval1 "(begin 1 2 3)")))
  (ok (eq nil (eval1 "(begin)"))))

;;; --- Coverage: eval-string with defaults ---
(deftest test-eval-string-defaults
  ;; Call eval-string with no keyword args to exercise default parameter values
  (ok (= 3 (eval-string "(+ 1 2)"))))

(deftest test-float-literals
  (ok (= 3.14d0 (eval1 "3.14")))
  (ok (= -0.5d0 (eval1 "-0.5")))
  (ok (= 1000.0d0 (eval1 "1e3")))
  (ok (= 0.5d0 (eval1 ".5")))
  (ok (= 2.5d-10 (eval1 "2.5e-10"))))

(deftest test-float-arithmetic
  (ok (= 4.0d0 (eval1 "(+ 1.5 2.5)")))
  (ok (= 6.0d0 (eval1 "(* 3.0 2)")))
  (ok (= 10.0d0 (eval1 "(- 10.5 0.5)")))
  ;; Mixed integer/float
  (ok (= 1.5d0 (eval1 "(+ 1 0.5)")))
  (ok (= 2.0d0 (eval1 "(* 2 1.0)"))))

(deftest test-float-division
  ;; / always returns float
  (ok (typep (eval1 "(/ 1 3)") 'double-float))
  (ok (< (abs (- (eval1 "(/ 22 7)") 3.142857d0)) 0.001d0))
  ;; Division by zero
  (ok (signals (eval1 "(/ 1 0)") 'wardlisp-type-error))
  (ok (signals (eval1 "(/ 1.0 0)") 'wardlisp-type-error)))

(deftest test-integer-predicate
  (ok (eq t (eval1 "(integer? 42)")))
  (ok (eq t (eval1 "(integer? -7)")))
  (ok (eq t (eval1 "(integer? 0)")))
  (ok (eq nil (eval1 "(integer? 3.14)")))
  (ok (eq nil (eval1 "(integer? 'x)")))
  (ok (eq nil (eval1 "(integer? nil)")))
  (ok (eq nil (eval1 "(integer? t)")))
  (ok (eq nil (eval1 "(integer? (cons 1 2))"))))

(deftest test-number-predicate
  (ok (eq t (eval1 "(number? 42)")))
  (ok (eq t (eval1 "(number? 3.14)")))
  (ok (eq t (eval1 "(number? -0.5)")))
  (ok (eq nil (eval1 "(number? 'x)")))
  (ok (eq nil (eval1 "(number? nil)")))
  (ok (eq nil (eval1 "(number? (list 1 2))"))))

(deftest test-comparison-non-number-returns-nil
  ;; = returns nil instead of erroring for non-numbers
  (ok (eq nil (eval1 "(= 'x 'y)")))
  (ok (eq nil (eval1 "(= 'x 1)")))
  (ok (eq nil (eval1 "(= nil nil)")))
  ;; Other comparisons return nil for non-numbers
  (ok (eq nil (eval1 "(< 'a 'b)")))
  (ok (eq nil (eval1 "(<= t nil)")))
  (ok (eq nil (eval1 "(> 'x 1)")))
  (ok (eq nil (eval1 "(>= nil 0)")))
  ;; Still works for numbers
  (ok (eq t (eval1 "(= 1 1)")))
  (ok (eq t (eval1 "(= 1.0 1)")))
  (ok (eq t (eval1 "(< 1 2.0)"))))

(deftest test-reserved-name-error-messages
  ;; t and nil cannot be used as variable names
  (ok (signals (eval1 "(let ((t 42)) t)") 'wardlisp-parse-error))
  (ok (signals (eval1 "(let ((nil 1)) nil)") 'wardlisp-parse-error))
  (ok (signals (eval1 "(let* ((t 1)) t)") 'wardlisp-parse-error))
  (ok (signals (eval1 "((lambda (t) t) 1)") 'wardlisp-parse-error))
  (ok (signals (eval1 "(define t 42)") 'wardlisp-parse-error))
  (ok (signals (eval1 "(define (t x) x)") 'wardlisp-parse-error))
  (ok (signals (eval1 "(define (f t) t)") 'wardlisp-parse-error)))
