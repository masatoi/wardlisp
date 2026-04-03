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

(defun evaluate (code &key (fuel 1000000) (max-depth 100) (max-cons 10000)
                      (max-output 1000) (max-integer (expt 2 64)) (timeout 5))
  "Evaluate CODE string in the restricted Lisp.
Returns (values result metrics-plist).
On error, returns (values nil metrics-plist) with :error-type and :error-message.
Note: NIL is a valid successful result.  Always check :error-type to distinguish
success from failure."
  ;; Validate inputs before creating execution context
  (unless (stringp code)
    (return-from evaluate
      (values nil (make-metrics nil
                   :error-type 'wardlisp-type-error
                   :error-message (format nil "evaluate: expected string, got ~a"
                                          (let ((tp (type-of code)))
                                            (if (consp tp) (car tp) tp)))))))
  (unless (and (integerp fuel) (plusp fuel))
    (return-from evaluate
      (values nil (make-metrics nil
                   :error-type 'wardlisp-type-error
                   :error-message "evaluate: :fuel must be a positive integer"))))
  (unless (and (integerp max-depth) (plusp max-depth))
    (return-from evaluate
      (values nil (make-metrics nil
                   :error-type 'wardlisp-type-error
                   :error-message "evaluate: :max-depth must be a positive integer"))))
  (unless (and (integerp max-cons) (plusp max-cons))
    (return-from evaluate
      (values nil (make-metrics nil
                   :error-type 'wardlisp-type-error
                   :error-message "evaluate: :max-cons must be a positive integer"))))
  (unless (and (integerp max-output) (plusp max-output))
    (return-from evaluate
      (values nil (make-metrics nil
                   :error-type 'wardlisp-type-error
                   :error-message "evaluate: :max-output must be a positive integer"))))
  (unless (and (integerp max-integer) (plusp max-integer))
    (return-from evaluate
      (values nil (make-metrics nil
                   :error-type 'wardlisp-type-error
                   :error-message "evaluate: :max-integer must be a positive integer"))))
  (unless (and (realp timeout) (plusp timeout))
    (return-from evaluate
      (values nil (make-metrics nil
                   :error-type 'wardlisp-type-error
                   :error-message "evaluate: :timeout must be a positive number"))))
  ;; Context is created after validation, bound outside handler-case
  ;; so error handlers can access metrics and output
  (let ((ctx (make-exec-ctx :fuel fuel :max-depth max-depth :max-cons max-cons
                            :max-output max-output :max-integer max-integer)))
    (handler-case
        (sb-ext:with-timeout timeout
          (let* ((program (wardlisp/src/reader:wardlisp-read-program code))
                 (env (make-initial-env))
                 (result (eval-program program env ctx)))
            (values result (make-metrics ctx))))
      (wardlisp-error (e)
        (values nil
                (make-metrics ctx :error-type (type-of e)
                              :error-message (wardlisp-error-message e))))
      (sb-ext:timeout ()
        (values nil
                (make-metrics ctx :error-type 'wardlisp-timeout-exceeded
                              :error-message
                              (format nil "Evaluation timed out after ~d second~:p"
                                      timeout))))
      (serious-condition (e)
        (values nil
                (make-metrics ctx :error-type 'wardlisp-internal-error
                              :error-message
                              (format nil "Internal error: ~a" (type-of e))))))))

(defun make-metrics (ctx &key error-type error-message)
  "Build metrics plist from execution context.  CTX may be nil if context creation itself failed."
  (if ctx
      (list :steps-used (exec-ctx-steps-used ctx)
            :max-depth-reached (exec-ctx-max-depth-reached ctx)
            :cons-allocated (exec-ctx-cons-count ctx)
            :output (copy-seq (exec-ctx-output ctx))
            :fuel-remaining (exec-ctx-fuel ctx)
            :error-type error-type
            :error-message error-message)
      (list :steps-used 0 :max-depth-reached 0 :cons-allocated 0
            :output "" :fuel-remaining 0
            :error-type error-type :error-message error-message)))
