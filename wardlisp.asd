(defsystem "wardlisp"
  :version "0.2.0"
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
                 (:file "integration-test"))))
  :description "Test system for wardlisp"
  :perform (test-op (op c) (symbol-call :rove :run c)))
