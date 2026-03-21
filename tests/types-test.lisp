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
  (let ((ctx (make-exec-ctx :fuel 4)))
    (consume-fuel ctx)
    (ok (= 3 (exec-ctx-fuel ctx)))
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
