(defpackage :wardlisp/src/types
  (:use :cl)
  (:export
   ;; Cons cell
   #:ocons #:make-ocons #:ocons-p #:ocons-ocar #:ocons-ocdr
   ;; Closure
   #:closure #:make-closure #:closure-p
   #:closure-params #:closure-body #:closure-env #:closure-name
   ;; Builtin
   #:builtin #:make-builtin #:builtin-p
   #:builtin-name #:builtin-func #:builtin-arity
   ;; Execution context
   #:exec-ctx #:make-exec-ctx
   #:exec-ctx-fuel #:exec-ctx-max-depth #:exec-ctx-current-depth
   #:exec-ctx-cons-count #:exec-ctx-max-cons
   #:exec-ctx-output #:exec-ctx-max-output
   #:exec-ctx-steps-used #:exec-ctx-max-depth-reached
   #:exec-ctx-max-integer
   #:exec-ctx-expr-depth #:exec-ctx-max-expr-depth
   ;; Conditions
   #:wardlisp-error #:wardlisp-error-message
   #:wardlisp-parse-error
   #:wardlisp-name-error
   #:wardlisp-type-error
   #:wardlisp-arity-error
   #:wardlisp-step-limit-exceeded
   #:wardlisp-recursion-limit-exceeded
   #:wardlisp-memory-limit-exceeded
   #:wardlisp-integer-limit-exceeded
   #:wardlisp-output-limit-exceeded
   #:wardlisp-timeout-exceeded
   #:wardlisp-internal-error
   ;; Helpers
   #:consume-fuel #:track-depth #:track-expr-depth #:track-cons #:check-integer))
(in-package :wardlisp/src/types)

;;; --- Custom cons cell for allocation counting ---

(defstruct (ocons (:constructor make-ocons (ocar ocdr)))
  "A cons cell in the restricted language. Separate from CL cons for counting."
  ocar
  ocdr)

;;; --- Closure ---

(defstruct (closure (:constructor make-closure (params body env &optional name)))
  "A user-defined function closure."
  (params nil :type list)
  body
  env
  (name nil))

;;; --- Builtin function ---

(defstruct (builtin (:constructor make-builtin (name func arity)))
  "A built-in function."
  (name "" :type string)
  func
  (arity nil))

;;; --- Execution context ---

(defstruct (exec-ctx (:constructor make-exec-ctx
                         (&key (fuel 10000) (max-depth 100)
                               (max-cons 10000) (max-output 1000)
                               (max-integer (expt 2 64))
                               (max-expr-depth 1000))))
  "Mutable execution context tracking resource consumption."
  (fuel 10000 :type integer)
  (max-depth 100 :type fixnum)
  (current-depth 0 :type fixnum)
  (cons-count 0 :type fixnum)
  (max-cons 10000 :type fixnum)
  (output (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)
          :type (array character (*)))
  (max-output 1000 :type fixnum)
  (steps-used 0 :type integer)
  (max-depth-reached 0 :type fixnum)
  (max-integer (expt 2 64) :type integer)
  (expr-depth 0 :type fixnum)
  (max-expr-depth 1000 :type fixnum))

;;; --- Error conditions ---

(define-condition wardlisp-error (error)
  ((message :initarg :message :reader wardlisp-error-message
            :initform ""))
  (:report (lambda (c s) (format s "~a" (wardlisp-error-message c)))))

(define-condition wardlisp-parse-error (wardlisp-error) ())
(define-condition wardlisp-name-error (wardlisp-error) ())
(define-condition wardlisp-type-error (wardlisp-error) ())
(define-condition wardlisp-arity-error (wardlisp-error) ())
(define-condition wardlisp-step-limit-exceeded (wardlisp-error) ())
(define-condition wardlisp-recursion-limit-exceeded (wardlisp-error) ())
(define-condition wardlisp-memory-limit-exceeded (wardlisp-error) ())
(define-condition wardlisp-integer-limit-exceeded (wardlisp-error) ())
(define-condition wardlisp-output-limit-exceeded (wardlisp-error) ())
(define-condition wardlisp-timeout-exceeded (wardlisp-error) ())
(define-condition wardlisp-internal-error (wardlisp-error) ())

;;; --- Resource control helpers ---

(defun consume-fuel (ctx &optional (amount 1))
  "Consume fuel from context. Signals step-limit-exceeded when exhausted."
  (decf (exec-ctx-fuel ctx) amount)
  (incf (exec-ctx-steps-used ctx) amount)
  (when (<= (exec-ctx-fuel ctx) 0)
    (error 'wardlisp-step-limit-exceeded
           :message (format nil "Step limit exceeded after ~d steps"
                            (exec-ctx-steps-used ctx)))))

(defun track-depth (ctx delta)
  "Adjust recursion depth. Signals recursion-limit-exceeded when too deep."
  (incf (exec-ctx-current-depth ctx) delta)
  (when (> (exec-ctx-current-depth ctx) (exec-ctx-max-depth-reached ctx))
    (setf (exec-ctx-max-depth-reached ctx) (exec-ctx-current-depth ctx)))
  (when (> (exec-ctx-current-depth ctx) (exec-ctx-max-depth ctx))
    (error 'wardlisp-recursion-limit-exceeded
           :message (format nil "Recursion depth ~d exceeds limit ~d"
                            (exec-ctx-current-depth ctx)
                            (exec-ctx-max-depth ctx)))))

(defun track-expr-depth (ctx delta)
  "Adjust expression depth. Signals recursion-limit-exceeded when too deep."
  (incf (exec-ctx-expr-depth ctx) delta)
  (when (> (exec-ctx-expr-depth ctx) (exec-ctx-max-expr-depth ctx))
    (error 'wardlisp-recursion-limit-exceeded
           :message (format nil "Expression nesting depth ~d exceeds limit ~d"
                            (exec-ctx-expr-depth ctx)
                            (exec-ctx-max-expr-depth ctx)))))

(defun track-cons (ctx &optional (count 1))
  "Track cons cell allocation. Signals memory-limit-exceeded when over."
  (incf (exec-ctx-cons-count ctx) count)
  (when (> (exec-ctx-cons-count ctx) (exec-ctx-max-cons ctx))
    (error 'wardlisp-memory-limit-exceeded
           :message (format nil "Cons allocation ~d exceeds limit ~d"
                            (exec-ctx-cons-count ctx)
                            (exec-ctx-max-cons ctx)))))

(defun check-integer (ctx value)
  "Check integer is within allowed range. Signals integer-limit-exceeded if not."
  (when (> (abs value) (exec-ctx-max-integer ctx))
    (error 'wardlisp-integer-limit-exceeded
           :message (format nil "Integer ~d exceeds limit ~d"
                            value (exec-ctx-max-integer ctx))))
  value)
