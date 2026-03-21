(defpackage omoikane-lisp/tests/main
  (:use :cl
        :omoikane-lisp
        :rove))
(in-package :omoikane-lisp/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :omoikane-lisp)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
