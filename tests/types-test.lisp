(defpackage :wardlisp/tests/types-test
  (:use :cl :rove :wardlisp/src/types))
(in-package :wardlisp/tests/types-test)

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
    (ok (= 1 (exec-ctx-fuel ctx)))
    ;; fuel=1: one more consume brings it to 0 (exactly exhausted, still ok)
    (consume-fuel ctx)
    (ok (= 0 (exec-ctx-fuel ctx)))
    ;; fuel=0: next consume brings it to -1 (exceeded)
    (ok (signals (consume-fuel ctx) 'wardlisp-step-limit-exceeded))))

(deftest test-track-depth
  (let ((ctx (make-exec-ctx :max-depth 2)))
    (track-depth ctx 1)
    (ok (= 1 (exec-ctx-current-depth ctx)))
    (track-depth ctx 1)
    (ok (signals (track-depth ctx 1) 'wardlisp-recursion-limit-exceeded))))

(deftest test-track-cons
  (let ((ctx (make-exec-ctx :max-cons 2)))
    (track-cons ctx)
    (ok (= 1 (exec-ctx-cons-count ctx)))
    (track-cons ctx)
    (ok (signals (track-cons ctx) 'wardlisp-memory-limit-exceeded))))

(deftest test-check-integer
  (let ((ctx (make-exec-ctx :max-integer 100)))
    (ok (= 50 (check-integer ctx 50)))
    (ok (signals (check-integer ctx 200) 'wardlisp-integer-limit-exceeded))))

;;; --- Coverage: builtin struct ---
(deftest test-builtin-creation
  (let ((b (make-builtin "test" #'identity 2)))
    (ok (builtin-p b))
    (ok (equal "test" (builtin-name b)))
    (ok (= 2 (builtin-arity b)))
    (ok (functionp (builtin-func b)))))

;;; --- Coverage: closure with name ---
(deftest test-closure-with-name
  (let ((c (make-closure '("x") '("+" "x" 1) nil "my-func")))
    (ok (equal "my-func" (closure-name c)))
    (ok (equal '("+" "x" 1) (closure-body c)))
    (ok (null (closure-env c)))))

(deftest test-tail-call-kind
  (let ((tc (make-tail-call :expr '(1 2 3) :env '((("x" . 1))) :kind :body)))
    (ok (tail-call-p tc))
    (ok (equal '(1 2 3) (tail-call-expr tc)))
    (ok (eq :body (tail-call-kind tc)))))

;;; --- Coverage: exec-ctx custom params ---
(deftest test-exec-ctx-custom
  (let ((ctx (make-exec-ctx :fuel 50 :max-depth 5 :max-cons 10
                            :max-output 20 :max-integer 999)))
    (ok (= 50 (exec-ctx-fuel ctx)))
    (ok (= 5 (exec-ctx-max-depth ctx)))
    (ok (= 10 (exec-ctx-max-cons ctx)))
    (ok (= 20 (exec-ctx-max-output ctx)))
    (ok (= 999 (exec-ctx-max-integer ctx)))
    (ok (= 0 (exec-ctx-steps-used ctx)))
    (ok (= 0 (exec-ctx-max-depth-reached ctx)))))

;;; --- Coverage: condition reporting ---
(deftest test-error-reporting
  (let ((e (make-condition 'wardlisp-parse-error :message "test error")))
    (ok (typep e 'wardlisp-error))
    (ok (equal "test error" (wardlisp-error-message e)))
    (ok (equal "test error" (format nil "~a" e)))))

;;; --- Coverage: track-depth records max ---
(deftest test-track-depth-max
  (let ((ctx (make-exec-ctx :max-depth 10)))
    (track-depth ctx 3)
    (ok (= 3 (exec-ctx-max-depth-reached ctx)))
    (track-depth ctx -2)
    (ok (= 1 (exec-ctx-current-depth ctx)))
    (ok (= 3 (exec-ctx-max-depth-reached ctx)))))
