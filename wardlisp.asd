(defsystem "wardlisp"
  :version "0.3.0"
  :author ""
  :license ""
  :depends-on ()
  :serial t
  :components ((:module "src"
                :serial t
                :components
                ((:file "types")
                 (:file "reader")
                 (:file "env")
                 (:file "builtins")
                 (:file "evaluator")
                 (:file "main"))))
  :description "Restricted Lisp evaluator for educational games"
  :in-order-to ((test-op (test-op "wardlisp/tests"))))

(defsystem "wardlisp/tests"
  :author ""
  :license ""
  :depends-on ("wardlisp"
               "rove")
  :serial t
  :components ((:module "tests"
                :serial t
                :components
                ((:file "types-test")
                 (:file "reader-test")
                 (:file "env-test")
                 (:file "evaluator-test")
                 (:file "safety-test")
                 (:file "integration-test")
                 (:file "string-test")
                 (:file "string-api-test"))))
  :description "Test system for wardlisp"
  :perform (test-op (op c) (symbol-call :rove :run c)))

;;; Executable cl-spec contracts on the public API (docs/specs.md).  Opt-in:
;;; "wardlisp" never depends on it, so the production system needs no cl-spec.
;;; cl-spec is not in Quicklisp; CI pins it in .github/workflows/ci.yml.
(defsystem "wardlisp/specs"
  :author ""
  :license ""
  :depends-on ("wardlisp"
               "cl-spec/check-it")
  :serial t
  :components ((:module "specs"
                :serial t
                :components
                ((:file "package")
                 (:file "generators")
                 (:file "api")
                 (:file "language")
                 (:file "runner"))))
  :description "Executable cl-spec contracts for wardlisp's public API")
