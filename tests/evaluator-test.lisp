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

(deftest test-builtin-div
  (ok (= 3 (eval1 "(div 7 2)"))))

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
