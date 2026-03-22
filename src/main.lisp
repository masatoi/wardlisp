(defpackage :wardlisp
  (:use :cl
        :wardlisp/src/types
        :wardlisp/src/evaluator
        :wardlisp/src/builtins)
  (:export #:evaluate #:print-value
           #:wardlisp-error #:wardlisp-error-message
           #:wardlisp-parse-error #:wardlisp-name-error
           #:wardlisp-type-error #:wardlisp-arity-error
           #:wardlisp-step-limit-exceeded
           #:wardlisp-recursion-limit-exceeded
           #:wardlisp-memory-limit-exceeded
           #:wardlisp-integer-limit-exceeded
           #:wardlisp-output-limit-exceeded
           #:wardlisp-timeout-exceeded))
(in-package :wardlisp)

(defun evaluate (code &key (fuel 10000) (max-depth 100)
                           (max-cons 10000) (max-output 1000)
                           (max-integer (expt 2 64))
                           (timeout 5))
  "Evaluate CODE string in the restricted Lisp.
Returns (values result metrics-plist).
On error, returns (values nil metrics-plist) with :error-type in metrics."
  (declare (ignore timeout))
  (let ((ctx (make-exec-ctx :fuel fuel :max-depth max-depth
                            :max-cons max-cons :max-output max-output
                            :max-integer max-integer)))
    (handler-case
        (let* ((program (wardlisp/src/reader:wardlisp-read-program code))
               (env (make-initial-env))
               (result (let ((r nil))
                         (dolist (expr program r)
                           (setf r (wardlisp-eval expr env ctx))))))
          (values result (make-metrics ctx)))
      (wardlisp-error (e)
        (values nil (make-metrics ctx
                                  :error-type (type-of e)
                                  :error-message (wardlisp-error-message e)))))))

(defun make-metrics (ctx &key error-type error-message)
  "Build metrics plist from execution context."
  (list :steps-used (exec-ctx-steps-used ctx)
        :max-depth-reached (exec-ctx-max-depth-reached ctx)
        :cons-allocated (exec-ctx-cons-count ctx)
        :output (copy-seq (exec-ctx-output ctx))
        :fuel-remaining (exec-ctx-fuel ctx)
        :error-type error-type
        :error-message error-message))
