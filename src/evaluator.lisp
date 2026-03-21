(defpackage :omoikane-lisp/src/evaluator
  (:use :cl
        :omoikane-lisp/src/types
        :omoikane-lisp/src/reader
        :omoikane-lisp/src/env
        :omoikane-lisp/src/builtins)
  (:export #:omoikane-eval #:eval-string))
(in-package :omoikane-lisp/src/evaluator)
