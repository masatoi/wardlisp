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

(deftest test-equal-deep
  (multiple-value-bind (result metrics)
      (evaluate "(equal? '(1 (2 3) 4) '(1 (2 3) 4))")
    (ok (eq t result))
    (ok (null (getf metrics :error-type)))))

(deftest test-equal-symbols
  (let ((result (evaluate "(equal? 'foo 'foo)")))
    (ok (eq t result))))

(deftest test-equal-symbol-lists
  (let ((result (evaluate "(equal? '(a b c) '(a b c))")))
    (ok (eq t result))))

(deftest test-equal-negative
  (let ((result (evaluate "(equal? '(1 2) '(1 3))")))
    (ok (null result))))

(deftest test-equal-grading-use-case
  (let ((result (evaluate "
    (define (insert x lst)
      (cond ((null? lst) (list x))
            ((<= x (car lst)) (cons x lst))
            (t (cons (car lst) (insert x (cdr lst))))))
    (define (my-sort lst)
      (if (null? lst) nil
          (insert (car lst) (my-sort (cdr lst)))))
    (equal? (my-sort '(3 1 4 1 5 9 2 6)) '(1 1 2 3 4 5 6 9))"
    :fuel 100000 :max-depth 200 :max-cons 10000)))
    (ok (eq t result))))

(deftest test-append-basic
  (let ((result (evaluate "(equal? (append '(1 2) '(3 4)) '(1 2 3 4))"
                          :max-cons 100)))
    (ok (eq t result))))

(deftest test-append-variadic
  (let ((result (evaluate "(equal? (append '(1) '(2) '(3)) '(1 2 3))"
                          :max-cons 100)))
    (ok (eq t result))))

(deftest test-apply-builtin
  (let ((result (evaluate "(apply + '(1 2 3))")))
    (ok (= 6 result))))

(deftest test-apply-closure
  (let ((result (evaluate "(apply (lambda (x y) (+ x y)) '(3 4))")))
    (ok (= 7 result))))

(deftest test-apply-improper-list
  "apply with dotted pair signals type-error"
  (multiple-value-bind (result metrics)
      (evaluate "(apply + '(1 . 2))")
    (declare (ignore result))
    (ok (eq 'wardlisp-type-error (getf metrics :error-type)))))

(deftest test-apply-non-list
  "apply with non-list arg signals type-error"
  (multiple-value-bind (result metrics)
      (evaluate "(apply + 42)")
    (declare (ignore result))
    (ok (eq 'wardlisp-type-error (getf metrics :error-type)))))

(deftest test-apply-arity-error
  "apply with wrong arg count signals arity-error"
  (multiple-value-bind (result metrics)
      (evaluate "(apply cons '(1))")
    (declare (ignore result))
    (ok (eq 'wardlisp-arity-error (getf metrics :error-type)))))

(deftest test-append-zero-args
  "(append) returns nil"
  (let ((result (evaluate "(append)")))
    (ok (null result))))

(deftest test-append-one-arg
  "(append '(1 2)) returns the list as-is"
  (let ((result (evaluate "(equal? (append '(1 2)) '(1 2))" :max-cons 100)))
    (ok (eq t result))))

(deftest test-append-last-arg-non-list
  "(append '(1) 2) creates dotted pair"
  (let ((result (evaluate "(equal? (cdr (append '(1) 2)) 2)" :max-cons 100)))
    (ok (eq t result))))

(deftest test-append-improper-prefix
  "append with improper list as non-last arg signals type-error"
  (multiple-value-bind (result metrics)
      (evaluate "(append (cons 1 2) '(3))" :max-cons 100)
    (declare (ignore result))
    (ok (eq 'wardlisp-type-error (getf metrics :error-type)))))

(deftest test-mod-negative-dividend
  "(mod -7 2) => -1 (same sign as dividend)"
  (let ((result (evaluate "(mod -7 2)")))
    (ok (= -1 result))))

(deftest test-mod-negative-divisor
  "(mod 7 -2) => 1 (same sign as dividend)"
  (let ((result (evaluate "(mod 7 -2)")))
    (ok (= 1 result))))

(deftest test-mod-both-negative
  "(mod -7 -2) => -1 (same sign as dividend)"
  (let ((result (evaluate "(mod -7 -2)")))
    (ok (= -1 result))))

(deftest test-quotient-negative-divisor
  "(quotient 7 -2) => -3, (quotient -7 -2) => 3 (truncation toward zero)"
  (let ((r1 (evaluate "(quotient 7 -2)"))
        (r2 (evaluate "(quotient -7 -2)")))
    (ok (= -3 r1))
    (ok (= 3 r2))))

(deftest test-length-improper-list
  "(length (cons 1 2)) signals type-error"
  (multiple-value-bind (result metrics)
      (evaluate "(length (cons 1 2))")
    (declare (ignore result))
    (ok (eq 'wardlisp-type-error (getf metrics :error-type)))))

(deftest test-float-display-format
  "Float display: trailing zeros removed, at least one decimal digit kept"
  (multiple-value-bind (result metrics)
      (evaluate "(begin (print 1000.0) (print 3.14) (print 2.0))")
    (declare (ignore result))
    (let ((output (getf metrics :output)))
      (ok (search "1000.0" output))
      (ok (search "3.14" output))
      (ok (search "2.0" output)))))

(deftest test-evaluate-invalid-params
  "evaluate rejects invalid keyword parameters"
  ;; negative fuel
  (multiple-value-bind (result metrics)
      (evaluate "(+ 1 2)" :fuel -1)
    (declare (ignore result))
    (ok (eq 'wardlisp-type-error (getf metrics :error-type))))
  ;; zero timeout
  (multiple-value-bind (result metrics)
      (evaluate "(+ 1 2)" :timeout 0)
    (declare (ignore result))
    (ok (eq 'wardlisp-type-error (getf metrics :error-type))))
  ;; non-integer max-depth
  (multiple-value-bind (result metrics)
      (evaluate "(+ 1 2)" :max-depth 3.5)
    (declare (ignore result))
    (ok (eq 'wardlisp-type-error (getf metrics :error-type))))
  ;; non-string code
  (multiple-value-bind (result metrics)
      (evaluate 42)
    (declare (ignore result))
    (ok (eq 'wardlisp-type-error (getf metrics :error-type)))))

(deftest test-define-in-and-rejected
  "define inside and signals parse-error"
  (multiple-value-bind (result metrics)
      (evaluate "(and (define x 1) x)")
    (declare (ignore result))
    (ok (eq 'wardlisp-parse-error (getf metrics :error-type)))))

(deftest test-define-in-or-rejected
  "define inside or signals parse-error"
  (multiple-value-bind (result metrics)
      (evaluate "(or (define x 1) x)")
    (declare (ignore result))
    (ok (eq 'wardlisp-parse-error (getf metrics :error-type)))))
