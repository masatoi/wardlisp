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
  "Bound numbers must be integers or finite floats within max-integer; a float
of any format becomes a double-float"
  (flet ((bound-error (value &rest options)
           (getf (nth-value 1 (apply #'wardlisp:evaluate "x"
                                     :bindings (list (cons "x" value)) options))
                 :error-type)))
    (ok (null (bound-error 42)))
    (ok (null (bound-error 2.5d0)))
    (ok (eq 'wardlisp-type-error (bound-error 1/3)))
    (ok (eq 'wardlisp-type-error (bound-error #C(1 2))))
    (ok (null (bound-error 2.5f0)))
    (ok (eql 2.5d0 (wardlisp:evaluate "x" :bindings '(("x" . 2.5f0)))))
    (ok (eq 'wardlisp-integer-limit-exceeded (bound-error 1000 :max-integer 10)))
    (ok (eq 'wardlisp-integer-limit-exceeded (bound-error -1000.0d0 :max-integer 10)))))

(defun nan ()
  "Return a quiet double-float NaN, made at run time with the invalid trap
masked (constant folding would trap at compile time)."
  (sb-int:with-float-traps-masked (:invalid)
    (let ((infinity (symbol-value 'sb-ext:double-float-positive-infinity)))
      (- infinity infinity))))

(deftest test-bindings-refuse-non-finite-floats
  "NaN and infinities are not wardlisp numbers"
  (flet ((bound-error (value)
           (getf (nth-value 1 (wardlisp:evaluate "(= x x)" :bindings (list (cons "x" value))))
                 :error-type)))
    (ok (eq 'wardlisp-type-error (bound-error (nan))))
    (ok (member (bound-error sb-ext:double-float-positive-infinity)
                '(wardlisp-type-error wardlisp-integer-limit-exceeded)))))

(deftest test-malformed-bindings-are-type-errors
  "A :bindings value that is not a proper alist is a type-error"
  (let ((circular (list (cons "x" 1))))
    (setf (cdr circular) circular)
    (dolist (bindings (list '(("x" . 1) . 5) circular 42 '(("x" . 1) 7)))
      (ok (eq 'wardlisp-type-error
              (getf (nth-value 1 (wardlisp:evaluate "1" :bindings bindings)) :error-type))))))

(defun timeout-refused-p (kind)
  "True when EVALUATE refuses the :timeout KIND names as a type-error.  The
value is built here so that no NaN reaches the values Rove records."
  (let ((timeout (ecase kind
                   (:huge-integer (expt 10 30))
                   (:huge-float 1d300)
                   (:infinity sb-ext:double-float-positive-infinity)
                   (:nan (nan))
                   (:zero 0)
                   (:negative -1)
                   (:string "5"))))
    (eq 'wardlisp-type-error
        (getf (nth-value 1 (wardlisp:evaluate "1" :timeout timeout)) :error-type))))

(deftest test-timeout-must-be-a-bounded-positive-number
  "A timeout beyond the maximum, infinite or NaN is refused, and refusing it
leaves later evaluations working"
  (dolist (kind '(:huge-integer :huge-float :infinity :nan :zero :negative :string))
    (ok (timeout-refused-p kind) (string kind)))
  (ok (eql 3 (wardlisp:evaluate "(+ 1 2)")))
  (ok (eql 3 (wardlisp:evaluate "(+ 1 2)" :timeout 1/2)))
  (ok (eql 3 (wardlisp:evaluate "(+ 1 2)" :timeout 1000000))))

(deftest test-evaluate-ignores-host-rounding-mode
  "Float results do not depend on the host's rounding mode"
  (let ((expected (list (wardlisp:evaluate "(/ 1 3)") (wardlisp:evaluate "(* 1.1 1.1)")))
        (saved (sb-int:get-floating-point-modes)))
    (unwind-protect
         (progn
           (sb-int:set-floating-point-modes :rounding-mode :positive-infinity)
           (ok (equal expected
                      (list (wardlisp:evaluate "(/ 1 3)") (wardlisp:evaluate "(* 1.1 1.1)"))))
           (ok (eq :positive-infinity
                   (getf (sb-int:get-floating-point-modes) :rounding-mode))))
      (apply #'sb-int:set-floating-point-modes saved))))

(deftest test-limits-may-be-any-positive-integer
  "Section 13.1: every limit is a positive integer, bignums included"
  (let ((huge (expt 10 100)))
    (dolist (option '(:fuel :max-depth :max-cons :max-output :max-integer))
      (ok (eql 3 (wardlisp:evaluate "(+ 1 2)" option huge)) (string option)))))

(deftest test-lengths-respect-max-integer
  "Section 3.1: an integer a builtin returns is subject to max-integer too"
  (flet ((error-type (code)
           (getf (nth-value 1 (wardlisp:evaluate code :max-integer 5)) :error-type)))
    (ok (eq 'wardlisp-integer-limit-exceeded (error-type "(string-length \"abcdefgh\")")))
    (ok (eq 'wardlisp-integer-limit-exceeded (error-type "(length '(1 1 1 1 1 1 1 1))")))
    (ok (null (error-type "(length '(1 2 3))")))))

(deftest test-negative-zero-float-literal
  "\"-0.\" is the float -0.0, like \"-0.0\""
  (ok (eql -0.0d0 (wardlisp:evaluate "-0.")))
  (ok (eql -0.0d0 (wardlisp:evaluate "-0.0"))))

(defparameter *dup-definition*
  "(define (dup n) (if (= n 0) 1 (let ((y (dup (- n 1)))) (list y y))))"
  "A value of N levels whose printed form has 2^N leaves, from 2N cells.")

(defun evaluate-quickly (code &rest options)
  "Evaluate CODE with OPTIONS and a short timeout; return the error type and the
seconds it took."
  (let ((start (get-internal-real-time)))
    (multiple-value-bind (value metrics)
        (apply #'wardlisp:evaluate (format nil "~A ~A" *dup-definition* code)
               :timeout 30 options)
      (declare (ignore value))
      (values (getf metrics :error-type)
              (/ (- (get-internal-real-time) start) internal-time-units-per-second)
              metrics))))

(deftest test-shared-structure-is-bounded-by-the-limits
  "Printing, describing and comparing a value with exponentially shared
structure is bounded by max-output, the message size and fuel, not by the size
of its printed form"
  (multiple-value-bind (type seconds) (evaluate-quickly "(print (dup 60))")
    (ok (eq 'wardlisp-output-limit-exceeded type))
    (ok (< seconds 2)))
  (multiple-value-bind (type seconds metrics) (evaluate-quickly "(+ (dup 60) 1)")
    (ok (eq 'wardlisp-type-error type))
    (ok (< seconds 2))
    (ok (< (length (getf metrics :error-message)) 300)))
  (multiple-value-bind (type seconds) (evaluate-quickly "(equal? (dup 40) (dup 40))")
    (ok (eq 'wardlisp-step-limit-exceeded type))
    (ok (< seconds 5))))

(deftest test-equal-charges-a-step-per-pair
  "equal? consumes one step per pair of cells it compares"
  (flet ((steps (code) (getf (nth-value 1 (wardlisp:evaluate code)) :steps-used)))
    (ok (= 3 (- (steps "(equal? '(1 2 3) '(1 2 3))") (steps "(equal? '() '())"))))))

(deftest test-long-quoted-list
  "A quoted list's length is limited by max-cons, not by the nesting limit"
  (let ((code (format nil "(length '(~{~D~^ ~}))" (loop for i below 1200 collect 0))))
    (ok (eql 1200 (wardlisp:evaluate code)))))

(deftest test-error-messages-use-wardlisp-syntax
  "Error messages show values and source in wardlisp syntax"
  (flet ((message (code) (getf (nth-value 1 (wardlisp:evaluate code)) :error-message)))
    (ok (equal "cond: each clause must be a list, got 1.5" (message "(cond 1.5)")))
    (ok (equal "let: binding name must be a symbol, got \"x\""
               (message "(let ((\"x\" 1)) 1)")))))

(deftest test-deeply-nested-quotes-are-parse-errors
  "Quote nesting counts towards the reader's nesting limit, like list nesting"
  (let ((code (concatenate 'string (make-string 60000 :initial-element #\') "x")))
    (ok (eq 'wardlisp-parse-error
            (getf (nth-value 1 (wardlisp:evaluate code)) :error-type)))))

(defun many-names (prefix count)
  "Return COUNT distinct symbol names PREFIX0 ... joined by spaces."
  (format nil "~{~A~^ ~}" (loop for i below count collect (format nil "~A~D" prefix i))))

(deftest test-wide-binding-forms-are-charged
  "Evaluating a lambda or a body's leading defines charges a step per name, so
a loop that re-evaluates a wide one is bounded by fuel and does linear work"
  (let ((lambda-loop
          (format nil "(define (spin n) (if (= n 0) 0 (begin (lambda (~A) 1) (spin (- n 1)))))
(spin 1000000)" (many-names "p" 500)))
        (define-loop
          (format nil "(define (spin n) ~{(define (~A) 1) ~}(if (= n 0) 0 (spin (- n 1))))
(spin 1000000)" (loop for i below 300 collect (format nil "d~D" i)))))
    (dolist (code (list lambda-loop define-loop))
      (ok (eq 'wardlisp-step-limit-exceeded
              (getf (nth-value 1 (wardlisp:evaluate code :fuel 200000)) :error-type)))))
  (flet ((steps (code) (getf (nth-value 1 (wardlisp:evaluate code)) :steps-used)))
    (ok (= 4 (- (steps "(lambda (a b c d e) 1)") (steps "(lambda (a) 1)"))))))

(deftest test-string-value-error-is-bounded
  "string-value's error message is cut like every other"
  (let ((value (wardlisp:evaluate (format nil "~A (dup 40)" *dup-definition*))))
    (ok (handler-case (progn (wardlisp:string-value value) nil)
          (wardlisp-type-error (e) (< (length (wardlisp-error-message e)) 300))))))

(deftest test-equal-depth-counts-nesting-not-length
  "equal? compares lists longer than its nesting limit"
  (ok (eq t (wardlisp:evaluate
             "(define (build n acc) (if (= n 0) acc (build (- n 1) (cons n acc))))
(equal? (build 10001 nil) (build 10001 nil))"
             :max-cons 100000))))

(deftest test-partial-results-respect-max-integer
  "Section 11.4: a variadic operation checks each partial result, so one step
never builds a number far beyond max-integer"
  (flet ((error-type (code)
           (getf (nth-value 1 (wardlisp:evaluate code :max-integer 1000)) :error-type)))
    (ok (eq 'wardlisp-integer-limit-exceeded (error-type "(* 100 100 0)")))
    (ok (eq 'wardlisp-integer-limit-exceeded (error-type "(+ 900 900 -900)")))
    (ok (null (error-type "(* 10 10 10)")))))

(deftest test-number-limit-messages-are-bounded
  "integer-limit-exceeded messages cut long numbers like other messages"
  (dolist (code '("1.5e300" "(* 1e300 1)"
                  "(define (mk n acc) (if (= n 0) acc (mk (- n 1) (cons 18446744073709551615 acc))))
(apply * (mk 9000 nil))"))
    (let ((metrics (nth-value 1 (wardlisp:evaluate code :max-integer (expt 2 64)))))
      (ok (eq 'wardlisp-integer-limit-exceeded (getf metrics :error-type)))
      (ok (< (length (getf metrics :error-message)) 450)))))

(deftest test-local-function-defines-are-charged-per-parameter
  "A body's local function define costs a step per parameter, like lambda"
  (flet ((steps (code) (getf (nth-value 1 (wardlisp:evaluate code)) :steps-used)))
    (ok (= 4 (- (steps "(define (h) (define (g a b c d e) 0) 1) (h)")
                (steps "(define (h) (define (g a) 0) 1) (h)"))))))

(deftest test-evaluate-ignores-host-break-on-signals
  "The host's *break-on-signals* does not make evaluate break on the errors it
classifies internally"
  (let ((*break-on-signals* 'error))
    (ok (eq 'wardlisp-type-error
            (getf (nth-value 1 (wardlisp:evaluate "(car 1)")) :error-type)))))

(deftest test-host-recursion-is-bounded-whatever-max-depth
  "Recursion through non-final body expressions or define initializers counts
towards the expression nesting limit, so a huge max-depth cannot exhaust the
host stack"
  (dolist (code '("(define (f n) (if (> n 0) (f (- n 1)) 0) n) (f 100000)"
                  "(define (f n) (define y (if (= n 0) 0 (f (- n 1)))) (+ y 1)) (f 100000)"
                  "(define (f n) (if (= n 0) 0 (+ 1 (f (- n 1))))) (f 100000)"))
    (ok (eq 'wardlisp-recursion-limit-exceeded
            (getf (nth-value 1 (wardlisp:evaluate code :max-depth 1000000 :fuel 100000000))
                  :error-type))
        code)))

(deftest test-host-arguments-are-checked-before-the-program
  "A malformed host argument is reported even when the program would not parse"
  (ok (eq 'wardlisp-type-error
          (getf (nth-value 1 (wardlisp:evaluate "(" :bindings 42)) :error-type))))

(deftest test-integer-division-is-exact-before-conversion
  "/ on two integers divides exactly, then converts the quotient to a float"
  (let ((huge (expt 10 1000))
        (x (format nil "(* ~{~A~^ ~})" (make-list 5 :initial-element (expt 10 80)))))
    (ok (eql 0.0d0 (wardlisp:evaluate (format nil "(/ 1 ~A)" x) :max-integer huge)))
    (ok (eql 1.0d0 (wardlisp:evaluate (format nil "(/ (+ ~A 1) ~A)" x x) :max-integer huge)))))

(deftest test-host-timeout-is-not-swallowed
  "A timeout the host sets around evaluate reaches the host"
  (ok (eq :host-timeout
          (handler-case
              (sb-ext:with-timeout 1
                (wardlisp:evaluate "(define (f) (f)) (f)" :timeout 100 :fuel (expt 10 12)))
            (sb-ext:timeout () :host-timeout)))))

(deftest test-long-names-in-messages-are-cut
  "Messages that echo a source token or name cut it at 200 characters"
  (let ((name (make-string 5000 :initial-element #\a)))
    (dolist (code (list name (format nil "~A:b" name)))
      (ok (< (length (getf (nth-value 1 (wardlisp:evaluate code)) :error-message)) 300)))))

(deftest test-tiny-timeouts-never-signal
  "A valid timeout far below a microsecond ends the evaluation as
timeout-exceeded (or lets it finish); it never escapes as a host condition"
  (dolist (timeout (list 1/1000000000 1d-300 1/2000000))
    (ok (loop repeat 100
              always (member (handler-case
                                 (getf (nth-value 1 (wardlisp:evaluate "(define (f) (f)) (f)"
                                                                       :timeout timeout
                                                                       :fuel (expt 10 12)))
                                       :error-type)
                               (serious-condition () :escaped))
                             '(wardlisp-timeout-exceeded nil)))
        (format nil "timeout ~S" timeout))))

(deftest test-host-timeout-at-the-same-deadline-is-delivered
  "A host timeout that expires together with evaluate's own still reaches the
host"
  (ok (eq :host-timeout
          (handler-case
              (sb-ext:with-timeout 1/2
                (wardlisp:evaluate "(define (f) (f)) (f)" :timeout 1/2 :fuel (expt 10 15))
                (sleep 2)
                :host-body-completed)
            (sb-ext:timeout () :host-timeout)))))

(defun correctly-rounded-subnormal (rational)
  "Return the double nearest to RATIONAL, a positive value below the smallest
normalized double, rounding ties to even."
  (scale-float (coerce (round (* rational (expt 2 1074))) 'double-float) -1074))

(deftest test-subnormal-literals-round-to-nearest
  "Float literals in the subnormal range read as the nearest double"
  (dolist (case '(("1.23456e-315" 123456 -320) ("7.777e-320" 7777 -323)
                  ("2.2250738585072e-308" 22250738585072 -321)))
    (destructuring-bind (text mantissa exponent) case
      (ok (eql (correctly-rounded-subnormal (* mantissa (expt 10 exponent)))
               (wardlisp:evaluate text))
          text))))

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
