(defpackage :omoikane-lisp/src/builtins
  (:use :cl :omoikane-lisp/src/types :omoikane-lisp/src/env)
  (:export #:make-initial-env #:print-value))
(in-package :omoikane-lisp/src/builtins)
