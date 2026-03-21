(defsystem "omoikane-lisp"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "omoikane-lisp/tests"))))

(defsystem "omoikane-lisp/tests"
  :author ""
  :license ""
  :depends-on ("omoikane-lisp"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for omoikane-lisp"
  :perform (test-op (op c) (symbol-call :rove :run c)))
