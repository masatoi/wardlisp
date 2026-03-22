(defpackage :wardlisp/tests/integration-test
  (:use :cl :rove :wardlisp :wardlisp/src/types))
(in-package :wardlisp/tests/integration-test)

;;; PRD Acceptance Criterion 1: user functions + recursion
(deftest test-factorial
  (multiple-value-bind (result metrics) (evaluate "
    (define (factorial n)
      (if (= n 0) 1 (* n (factorial (- n 1)))))
    (factorial 10)")
    (ok (= 3628800 result))
    (ok (> (getf metrics :steps-used) 0))))

(deftest test-fibonacci
  (let ((result (evaluate "
    (define (fib n)
      (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
    (fib 10)")))
    (ok (= 55 result))))

(deftest test-list-reverse
  (let ((result (evaluate "
    (define (rev lst acc)
      (if (null? lst) acc
          (rev (cdr lst) (cons (car lst) acc))))
    (rev (list 1 2 3 4 5) nil)")))
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

;;; PRD Acceptance Criterion 2: infinite recursion stops
(deftest test-infinite-recursion-halts
  (multiple-value-bind (result metrics)
      (evaluate "(define (loop) (loop)) (loop)" :fuel 100)
    (ok (null result))
    (ok (eq 'wardlisp-step-limit-exceeded (getf metrics :error-type)))))

;;; PRD Acceptance Criterion 3: huge list stops
(deftest test-huge-list-halts
  (multiple-value-bind (result metrics)
      (evaluate "
        (define (huge n acc)
          (if (= n 0) acc (huge (- n 1) (cons n acc))))
        (huge 10000 nil)"
        :max-cons 50 :fuel 100000)
    (ok (null result))
    (ok (eq 'wardlisp-memory-limit-exceeded (getf metrics :error-type)))))

;;; PRD Acceptance Criterion 5: no file/network/OS
(deftest test-no-file-access
  (multiple-value-bind (result metrics)
      (evaluate "(open foo)")
    (ok (null result))
    (ok (eq 'wardlisp-name-error (getf metrics :error-type)))))

(deftest test-no-system-access
  (multiple-value-bind (result metrics)
      (evaluate "(run-program ls)")
    (ok (null result))
    (ok (eq 'wardlisp-name-error (getf metrics :error-type)))))

;;; PRD Acceptance Criterion 6: metrics returned
(deftest test-metrics-returned
  (multiple-value-bind (result metrics)
      (evaluate "(+ 1 2)")
    (ok (= 3 result))
    (ok (getf metrics :steps-used))
    (ok (integerp (getf metrics :steps-used)))
    (ok (integerp (getf metrics :max-depth-reached)))
    (ok (integerp (getf metrics :cons-allocated)))))

;;; Real programs
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

;;; --- Coverage: evaluate returns metrics on various errors ---
(deftest test-evaluate-parse-error
  (multiple-value-bind (result metrics)
      (evaluate "(+ 1")
    (ok (null result))
    (ok (eq 'wardlisp-parse-error (getf metrics :error-type)))
    (ok (stringp (getf metrics :error-message)))))

(deftest test-evaluate-type-error
  (multiple-value-bind (result metrics)
      (evaluate "(+ 1 t)")
    (ok (null result))
    (ok (eq 'wardlisp-type-error (getf metrics :error-type)))))

(deftest test-evaluate-arity-error
  (multiple-value-bind (result metrics)
      (evaluate "((lambda (x) x) 1 2)")
    (ok (null result))
    (ok (eq 'wardlisp-arity-error (getf metrics :error-type)))))

;;; --- Coverage: evaluate with output ---
(deftest test-evaluate-output
  (multiple-value-bind (result metrics)
      (evaluate "(print 42)")
    (ok (= 42 result))
    (ok (stringp (getf metrics :output)))
    (ok (search "42" (getf metrics :output)))))

;;; --- Coverage: evaluate with all metrics ---
(deftest test-evaluate-full-metrics
  (multiple-value-bind (result metrics)
      (evaluate "(cons 1 (cons 2 nil))")
    (ok (ocons-p result))
    (ok (>= (getf metrics :cons-allocated) 2))
    (ok (>= (getf metrics :fuel-remaining) 0))
    (ok (null (getf metrics :error-type)))
    (ok (null (getf metrics :error-message)))))

;;; --- Coverage: evaluate with defaults (exercises default keyword params) ---
(deftest test-evaluate-defaults
  ;; Call evaluate with only code, no keyword args, to exercise default params
  (multiple-value-bind (result metrics)
      (evaluate "(+ 1 1)")
    (ok (= 2 result))
    (ok (integerp (getf metrics :steps-used)))))

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
    :fuel 5000000 :max-depth 200)))
    (ok (= 0 result))))
