(defpackage :wardlisp/src/env
  (:use :cl :wardlisp/src/types)
  (:export #:env-extend #:env-lookup #:env-set!))
(in-package :wardlisp/src/env)

(defun env-extend (parent names values)
  "Create a new environment frame extending PARENT with NAMES bound to VALUES."
  (let ((frame (mapcar #'cons names values)))
    (cons frame parent)))

(defun env-lookup (env name)
  "Look up NAME in ENV. Signals wardlisp-name-error if not found."
  (loop for frame in env
        do (let ((pair (assoc name frame :test #'string=)))
             (when pair
               (let ((value (cdr pair)))
                 (when (eq value +uninitialized-binding+)
                   (error 'wardlisp-name-error
                          :message (format nil "Uninitialized local binding: ~a" name)))
                 (return-from env-lookup value)))))
  (error 'wardlisp-name-error
         :message (format nil "Undefined variable: ~a" name)))

(defun env-set! (env name value)
  "Set NAME to VALUE in the first frame where it's found."
  (loop for frame in env
        do (let ((pair (assoc name frame :test #'string=)))
             (when pair
               (setf (cdr pair) value)
               (return-from env-set! value))))
  (error 'wardlisp-name-error
         :message (format nil "Cannot set undefined variable: ~a" name)))
