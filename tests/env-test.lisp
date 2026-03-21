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
