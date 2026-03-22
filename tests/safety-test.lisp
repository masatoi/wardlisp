(defpackage :wardlisp/tests/safety-test
  (:use :cl :rove
        :wardlisp/src/types
        :wardlisp/src/evaluator))

(in-package :wardlisp/tests/safety-test)

(defun eval-safe (code &key (fuel 100) (max-depth 10) (max-cons 50)
                            (max-output 100) (max-integer 1000000))
  (eval-string code :fuel fuel :max-depth max-depth :max-cons max-cons
                    :max-output max-output :max-integer max-integer))

;; --- Step limit ---
(deftest test-infinite-recursion-stops
  (ok (signals
       (eval-safe "(define (f x) (f x)) (f 1)" :max-depth 1000)
       'wardlisp-step-limit-exceeded)))

(deftest test-deep-computation-stops
  (ok (signals
       (eval-safe "(define (f n) (if (= n 0) 0 (+ 1 (f (- n 1))))) (f 10000)"
                  :fuel 50)
       'wardlisp-step-limit-exceeded)))

;; --- Recursion depth ---
(deftest test-deep-recursion-stops
  (ok (signals
       (eval-safe "(define (f n) (if (= n 0) 0 (+ 1 (f (- n 1))))) (f 100)"
                  :fuel 100000 :max-depth 5)
       'wardlisp-recursion-limit-exceeded)))

;; --- Memory / cons limit ---

(deftest test-huge-list-stops
  (ok (signals
       (eval-safe "(define (make n acc) (if (= n 0) acc (make (- n 1) (cons n acc)))) (make 1000 nil)"
                  :fuel 100000 :max-cons 10 :max-depth 2000)
       'wardlisp-memory-limit-exceeded)))

;; --- Integer size limit ---
(deftest test-huge-integer-stops
  (ok (signals
       (eval-safe "(* 999999 999999)" :max-integer 1000000)
       'wardlisp-integer-limit-exceeded)))

;; --- Output limit ---
(deftest test-output-limit
  (ok (signals
       (eval-safe "(define (spam n) (if (= n 0) nil (let () (print n) (spam (- n 1))))) (spam 100)"
                  :fuel 100000 :max-output 20)
       'wardlisp-output-limit-exceeded)))

;; --- Name error ---
(deftest test-undefined-variable
  (ok (signals (eval-safe "undefined-var") 'wardlisp-name-error)))

;; --- Type error ---
(deftest test-type-error-arithmetic
  (ok (signals (eval-safe "(+ 1 t)") 'wardlisp-type-error)))

(deftest test-type-error-car
  (ok (signals (eval-safe "(car 42)") 'wardlisp-type-error)))

;; --- Arity error ---
(deftest test-arity-error
  (ok (signals (eval-safe "((lambda (x) x) 1 2)") 'wardlisp-arity-error)))

;; --- No host eval ---
(deftest test-no-eval-access
  (ok (signals (eval-safe "(eval '(+ 1 2))") 'wardlisp-name-error)))

;; --- No setf/setq ---
(deftest test-no-mutation
  (ok (signals (eval-safe "(setf x 1)") 'wardlisp-name-error))
  (ok (signals (eval-safe "(setq x 1)") 'wardlisp-name-error)))
