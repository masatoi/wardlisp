;;;; specs/package.lisp
;;;;
;;;; Executable cl-spec contracts for wardlisp's public API.  This system is
;;;; opt-in: the production system "wardlisp" never depends on it, so loading
;;;; wardlisp does not pull in cl-spec.  See docs/specs.md.

(defpackage :wardlisp/specs
  (:use :cl)
  (:import-from :cl-spec
                #:defspec #:defspec-function #:defproperty #:defgenerator)
  (:import-from :wardlisp
                #:evaluate #:print-value
                #:ocons-p #:ocons-ocar #:ocons-ocdr
                #:string-value-p #:string-value #:make-string-value
                #:symbol-value-p #:number-value-p
                #:wardlisp-type-error)
  (:import-from :wardlisp/src/types
                #:make-ocons #:wstring-p #:wstring-value #:closure-p #:builtin-p)
  (:import-from :wardlisp/src/reader
                #:wardlisp-read)
  (:export #:contract-names
           #:property-names
           #:run-bundle
           #:run-negative-control
           #:main
           #:*documented-error-types*
           #:metrics-plist-p))
