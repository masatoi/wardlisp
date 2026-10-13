;;;; scripts/check-specs.lisp
;;;;
;;;; Command-line entry for the wardlisp/specs bundle.  CI runs it in a fresh
;;;; process; so can you:
;;;;
;;;;   ros run --load scripts/check-specs.lisp
;;;;   WARDLISP_SPECS_MODE=negative-control ros run --load scripts/check-specs.lisp
;;;;
;;;; WARDLISP_SPECS_MODE    check (default): every Function Spec and Property
;;;;                        under the fixed seeds;
;;;;                        negative-control: swap in known-wrong
;;;;                        implementations and demand that the bundle catches
;;;;                        each one (this process only -- it replaces
;;;;                        production functions while it runs).
;;;; WARDLISP_SPECS_REPORT  file to write the report to, as one Lisp form.
;;;;
;;;; The script puts THIS checkout first in ASDF's search and refuses to run
;;;; unless wardlisp was loaded from it: a green result from another copy is not
;;;; evidence about this one.  cl-spec and check-it must already be findable,
;;;; through Quicklisp, Roswell's local-projects or CL_SOURCE_REGISTRY.
;;;;
;;;; Exit status: 0 passed; 1 the checks ran and something failed; 2 the script
;;;; could not run them (a load error, an unknown mode, a foreign checkout).

;;; First, before anything can signal: under `ros run` with no terminal the
;;; debugger reads end of file and the process exits 0, which would turn a
;;; script that never ran its checks into a green CI step.
#+sbcl (sb-ext:disable-debugger)

(require "asdf")

(defpackage :wardlisp/scripts/check-specs
  (:use :cl))

(in-package :wardlisp/scripts/check-specs)

(defparameter *checkout*
  (uiop:pathname-parent-directory-pathname
   (uiop:pathname-directory-pathname
    (or *load-truename* (error "Load scripts/check-specs.lisp as a file."))))
  "The wardlisp checkout this script belongs to, and the one it checks.")

(defun ensure-quicklisp ()
  "Load Quicklisp from its usual places unless it is loaded already."
  (unless (find-package "QL")
    (let ((setup (find-if #'probe-file
                          (list (merge-pathnames "quicklisp/setup.lisp"
                                                 (user-homedir-pathname))
                                (merge-pathnames ".roswell/lisp/quicklisp/setup.lisp"
                                                 (user-homedir-pathname))))))
      (when setup
        (load setup)))))

(defun load-system* (name)
  "Load the ASDF system NAME, through Quicklisp when it is present so that
missing dependencies are fetched."
  (if (find-package "QL")
      (uiop:symbol-call :ql :quickload name :silent t)
      (asdf:load-system name)))

(defun same-directory-p (a b)
  "True when the directories A and B are the same once symlinks are resolved."
  (equal (namestring (truename a)) (namestring (truename b))))

(defun parse-mode (text)
  "Return the runner mode WARDLISP_SPECS_MODE's TEXT names, or NIL."
  (cond ((or (null text) (string= text "") (string= text "check")) :check)
        ((string= text "negative-control") :negative-control)
        (t nil)))

(defun main ()
  "Run the mode WARDLISP_SPECS_MODE names and exit with its code."
  (handler-bind ((error (lambda (condition)
                          (format *error-output* "~&check-specs: ~A~%" condition)
                          (uiop:print-backtrace :stream *error-output* :count 40)
                          (uiop:quit 2))))
    (let ((mode (parse-mode (uiop:getenv "WARDLISP_SPECS_MODE")))
          (report (uiop:getenv "WARDLISP_SPECS_REPORT")))
      (unless mode
        (format *error-output* "~&check-specs: WARDLISP_SPECS_MODE must be check or ~
negative-control; got ~S~%" (uiop:getenv "WARDLISP_SPECS_MODE"))
        (uiop:quit 2))
      (push *checkout* asdf:*central-registry*)
      (ensure-quicklisp)
      (load-system* "wardlisp/specs")
      (let ((loaded (asdf:system-source-directory "wardlisp")))
        (format t "~&wardlisp: ~A~%cl-spec: ~A~%"
                loaded (asdf:system-source-directory "cl-spec"))
        (unless (same-directory-p loaded *checkout*)
          (format *error-output* "~&check-specs: wardlisp was loaded from ~A, not from ~
this checkout ~A~%" loaded *checkout*)
          (uiop:quit 2)))
      (uiop:quit (uiop:symbol-call :wardlisp/specs :main
                                   :mode mode
                                   :report-pathname
                                   (and report (plusp (length report))
                                        (uiop:parse-native-namestring report)))))))

(main)
