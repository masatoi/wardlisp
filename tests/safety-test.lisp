(defpackage :wardlisp/tests/safety-test
  (:use :cl :rove
        :wardlisp/src/types
        :wardlisp/src/evaluator))

(in-package :wardlisp/tests/safety-test)

(defun eval-safe (code &key (fuel 100) (max-depth 10) (max-cons 50)
                            (max-output 100) (max-integer 1000000)
                            (max-expr-depth 1000))
  (eval-string code :fuel fuel :max-depth max-depth :max-cons max-cons
                    :max-output max-output :max-integer max-integer
                    :max-expr-depth max-expr-depth))

;; --- Step limit ---
(deftest test-infinite-recursion-stops
  (ok (signals
       (eval-safe "(define (f x) (f x)) (f 1)" :max-depth 1000)
       'wardlisp-step-limit-exceeded)))

(deftest test-deep-computation-stops
  (ok (signals
       (eval-safe "(define (f n) (if (= n 0) 0 (+ 1 (f (- n 1))))) (f 10000)"
                  :fuel 50)
       'wardlisp-step-limit-exceeded)))

;; --- Recursion depth ---
(deftest test-deep-recursion-stops
  (ok (signals
       (eval-safe "(define (f n) (if (= n 0) 0 (+ 1 (f (- n 1))))) (f 100)"
                  :fuel 100000 :max-depth 5)
       'wardlisp-recursion-limit-exceeded)))

;; --- Memory / cons limit ---

(deftest test-huge-list-stops
  (ok (signals
       (eval-safe "(define (make n acc) (if (= n 0) acc (make (- n 1) (cons n acc)))) (make 1000 nil)"
                  :fuel 100000 :max-cons 10 :max-depth 2000)
       'wardlisp-memory-limit-exceeded)))

;; --- Integer size limit ---
(deftest test-huge-integer-stops
  (ok (signals
       (eval-safe "(* 999999 999999)" :max-integer 1000000)
       'wardlisp-integer-limit-exceeded)))

(deftest test-huge-float-stops
  "Float exceeding max-integer triggers integer-limit-exceeded"
  (ok (signals
       (eval-safe "(* 999999.0 999999.0)" :max-integer 1000000)
       'wardlisp-integer-limit-exceeded)))

(deftest test-float-overflow-is-integer-limit
  "A float result beyond double-float range is integer-limit-exceeded even when
max-integer is larger than any double-float"
  (let ((huge (expt 10 1000))
        (big-int (format nil "(* ~{~A~^ ~})" (make-list 4 :initial-element
                                                          (format nil "1~94,,,'0@A" 0)))))
    (dolist (code (list "(* 1e300 1e300)" "(+ 1e308 1e308)" "(- -1e308 1e308)"
                        "(/ 1e300 1e-300)" "(* 1e200 1e200 0)"
                        (format nil "(+ ~A 0.5)" big-int)
                        (format nil "(/ (+ ~A 1) 3)" big-int)))
      (ok (signals (eval-safe code :fuel 1000 :max-integer huge)
                   'wardlisp-integer-limit-exceeded)
          code))))

(deftest test-evaluate-never-reports-internal-error-for-float-edges
  "Float edge cases stay inside wardlisp's own error types through EVALUATE"
  (dolist (case `(("1e400" . wardlisp-parse-error)
                  ("(= 1e400 1)" . wardlisp-parse-error)
                  ("(* 1e300 1e300)" . wardlisp-integer-limit-exceeded)))
    (let ((metrics (nth-value 1 (wardlisp:evaluate (car case) :max-integer (expt 10 1000)))))
      (ok (eq (cdr case) (getf metrics :error-type)) (car case)))))

(defun call-with-host-float-traps (traps thunk)
  "Call THUNK with the host's float traps set to TRAPS, restoring them after."
  (let ((saved (sb-int:get-floating-point-modes)))
    (unwind-protect
         (progn (sb-int:set-floating-point-modes :traps traps)
                (funcall thunk))
      (apply #'sb-int:set-floating-point-modes saved))))

(deftest test-evaluate-ignores-host-float-traps
  "EVALUATE pins its own float modes: an underflow rounds to zero and an overflow
is integer-limit-exceeded whatever traps the host has enabled"
  (let ((huge (expt 10 1000)))
    (flet ((outcome (code)
             (multiple-value-bind (value metrics) (wardlisp:evaluate code :max-integer huge)
               (list value (getf metrics :error-type))))
           (host-traps () (getf (sb-int:get-floating-point-modes) :traps)))
      (dolist (traps '((:overflow :invalid :divide-by-zero :underflow) (:invalid) ()))
        (let ((before (host-traps))
              (label (format nil "host traps ~S" traps)))
          (call-with-host-float-traps
           traps
           (lambda ()
             (ok (equal '(0.0d0 nil) (outcome "(* 1e-300 1e-300)")) label)
             (ok (equal '(0.0d0 nil) (outcome "1e-400")) label)
             (ok (equal '(nil wardlisp-integer-limit-exceeded) (outcome "(* 1e300 1e300)"))
                 label)
             (ok (equal (sort (copy-list traps) #'string<)
                        (sort (copy-list (host-traps)) #'string<))
                 "the host's traps are restored")))
          (ok (equal before (host-traps))))))))

(deftest test-evaluate-ignores-host-reader-settings
  "The host's reader and printer variables do not change what a program means
or how its errors are reported"
  (let ((standard (nth-value 1 (wardlisp:evaluate "(* 99999 99999)" :max-integer 1000))))
    (let ((*print-base* 16) (*print-radix* t))
      (ok (equal standard
                 (nth-value 1 (wardlisp:evaluate "(* 99999 99999)" :max-integer 1000))))))
  (let ((*read-base* 16))
    (ok (eql 100000.0d0 (wardlisp:evaluate "1e5")))
    (ok (eq 'wardlisp-parse-error
            (getf (nth-value 1 (wardlisp:evaluate "(+ 1e400 0)")) :error-type)))))

(deftest test-bindings-accept-only-wardlisp-numbers
  "Bound numbers must be integers or double-floats within max-integer"
  (flet ((bound-error (value &rest options)
           (getf (nth-value 1 (apply #'wardlisp:evaluate "x"
                                     :bindings (list (cons "x" value)) options))
                 :error-type)))
    (ok (null (bound-error 42)))
    (ok (null (bound-error 2.5d0)))
    (ok (eq 'wardlisp-type-error (bound-error 1/3)))
    (ok (eq 'wardlisp-type-error (bound-error #C(1 2))))
    (ok (eq 'wardlisp-type-error (bound-error 2.5f0)))
    (ok (eq 'wardlisp-integer-limit-exceeded (bound-error 1000 :max-integer 10)))
    (ok (eq 'wardlisp-integer-limit-exceeded (bound-error -1000.0d0 :max-integer 10)))))

;; --- Output limit ---
(deftest test-output-limit
  (ok (signals
       (eval-safe "(define (spam n) (if (= n 0) nil (let () (print n) (spam (- n 1))))) (spam 100)"
                  :fuel 100000 :max-output 20)
       'wardlisp-output-limit-exceeded)))

;; --- Name error ---
(deftest test-undefined-variable
  (ok (signals (eval-safe "undefined-var") 'wardlisp-name-error)))

;; --- Type error ---
(deftest test-type-error-arithmetic
  (ok (signals (eval-safe "(+ 1 t)") 'wardlisp-type-error)))

(deftest test-type-error-car
  (ok (signals (eval-safe "(car 42)") 'wardlisp-type-error)))

;; --- Arity error ---
(deftest test-arity-error
  (ok (signals (eval-safe "((lambda (x) x) 1 2)") 'wardlisp-arity-error)))

;; --- No host eval ---
(deftest test-no-eval-access
  (ok (signals (eval-safe "(eval '(+ 1 2))") 'wardlisp-name-error)))

;; --- No setf/setq ---
(deftest test-no-mutation
  (ok (signals (eval-safe "(setf x 1)") 'wardlisp-name-error))
  (ok (signals (eval-safe "(setq x 1)") 'wardlisp-name-error)))

;; --- Parse depth limit ---
(deftest test-deep-nesting-parse-stops
  (ok (signals
       (eval-safe
        (concatenate 'string
          (make-string 10000 :initial-element #\()
          "1"
          (make-string 10000 :initial-element #\)))
        :fuel 1000000)
       'wardlisp-parse-error)))

(deftest test-deep-quoted-list-stops
  (ok (signals
       (eval-safe
        (concatenate 'string
          "'("
          (make-string 1000 :initial-element #\()
          "1"
          (make-string 1000 :initial-element #\))
          ")")
        :fuel 1000000 :max-cons 1000000)
       'wardlisp-error)))

(deftest test-deep-if-nesting-stops
  (ok (signals
       (eval-safe
        (format nil "~{~a~}1~{~a~}"
          (loop repeat 800 collect "(if t ")
          (loop repeat 800 collect ")"))
        :fuel 1000000
        :max-expr-depth 100)
       'wardlisp-error)))

(deftest test-reject-special-characters
  ;; Double-quote now starts a string literal (covered in string-test); the
  ;; other reader-reserved characters remain rejected.
  (ok (signals (eval-safe "`x") 'wardlisp-parse-error))
  (ok (signals (eval-safe ",x") 'wardlisp-parse-error))
  (ok (signals (eval-safe "\\x") 'wardlisp-parse-error))
  (ok (signals (eval-safe "|x|") 'wardlisp-parse-error)))

;; --- Reader attack regression tests ---
(deftest test-reader-blocks-hash-dot
  (ok (signals (eval-safe "#.(+ 1 2)") 'wardlisp-parse-error)))

(deftest test-reader-blocks-hash-quote
  (ok (signals (eval-safe "#'car") 'wardlisp-parse-error)))

(deftest test-reader-blocks-hash-paren
  (ok (signals (eval-safe "#(1 2 3)") 'wardlisp-parse-error)))

(deftest test-reader-blocks-package-prefix
  (ok (signals (eval-safe "cl:open") 'wardlisp-parse-error))
  (ok (signals (eval-safe "sb-ext:run-program") 'wardlisp-parse-error)))

(deftest test-reader-blocks-keyword
  (ok (signals (eval-safe ":keyword") 'wardlisp-parse-error)))

;; --- Evaluator escape attempt regression tests ---

(deftest test-apply-is-safe
  (ok (eql 3 (eval-safe "(apply + '(1 2))" :fuel 1000))))

(deftest test-no-funcall
  (ok (signals (eval-safe "(funcall + 1 2)") 'wardlisp-name-error)))

(deftest test-no-load
  (ok (signals (eval-safe "(load foo)") 'wardlisp-name-error)))

(deftest test-no-compile
  (ok (signals (eval-safe "(compile nil)") 'wardlisp-name-error)))

(deftest test-no-intern
  (ok (signals (eval-safe "(intern x)") 'wardlisp-name-error)))

;; --- Builtin integrity ---
(deftest test-builtin-shadowed-by-define
  (ok (eql 42 (eval-safe "(define + 42) +" :fuel 1000)))
  (ok (signals (eval-safe "(define + 42) (+ 1 2)" :fuel 1000) 'wardlisp-type-error)))

(deftest test-builtin-shadowed-by-let-is-safe
  (ok (signals (eval-safe "(let ((+ 42)) (+ 1 2))") 'wardlisp-type-error)))

(deftest test-timeout-exceeded
  "Infinite loop triggers timeout-exceeded error"
  (multiple-value-bind (result metrics)
      (wardlisp:evaluate "(define (loop) (loop)) (loop)"
                         :timeout 0.1 :fuel 100000000)
    (declare (ignore result))
    (ok (eq 'wardlisp/src/types:wardlisp-timeout-exceeded
            (getf metrics :error-type)))))
