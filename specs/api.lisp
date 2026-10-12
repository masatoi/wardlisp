;;;; specs/api.lisp
;;;;
;;;; Function Specs on wardlisp's public API: one contract per function, each
;;;; registered on the production symbol itself.  Clauses cite the section of
;;;; docs/language-spec.md they come from.

(in-package :wardlisp/specs)

;;; --- evaluate (sections 11, 12, 13.1, 13.2) ---

(defun error-report-consistent-p (value metrics)
  "Section 13.1/13.2: success has no :ERROR-TYPE and no :ERROR-MESSAGE; a
failure returns NIL, names a documented error type and carries a message."
  (let ((type (getf metrics :error-type))
        (message (getf metrics :error-message)))
    (if type
        (and (null value)
             (member type *documented-error-types*)
             (stringp message))
        (null message))))

(defun host-errors-contained-p (metrics)
  "Sections 1 and 12: no program, however malformed, makes EVALUATE report
INTERNAL-ERROR.  Every failure a program can cause is one of wardlisp's own
error types; INTERNAL-ERROR means a host condition escaped the sandbox's
classification, which is a defect of the implementation."
  (not (eq (getf metrics :error-type) 'wardlisp/src/types:wardlisp-internal-error)))

(defun result-is-wardlisp-value-p (value metrics)
  "Sections 1 and 3: a successful run returns a wardlisp value, never a host
object such as a CL symbol, ratio or complex."
  (or (getf metrics :error-type)
      (wardlisp-value-p value)))

(defun fuel-accounted-p (metrics fuel)
  "Section 11.1: every step consumed is taken from FUEL, so steps used and fuel
remaining always add up to the fuel given."
  (= fuel (+ (getf metrics :steps-used) (getf metrics :fuel-remaining))))

(defun output-within-limit-p (metrics max-output)
  "Section 11.5: the output buffer never holds more than MAX-OUTPUT characters,
whether or not the run failed."
  (<= (length (getf metrics :output)) max-output))

(defun success-within-limits-p (metrics fuel max-depth max-cons)
  "Section 11: a run that succeeded stayed inside every counted limit."
  (or (getf metrics :error-type)
      (and (<= (getf metrics :steps-used) fuel)
           (<= (getf metrics :max-depth-reached) max-depth)
           (<= (getf metrics :cons-allocated) max-cons))))

(defun limit-error-witnessed-p (metrics fuel max-cons)
  "Section 11.1/11.3: a step or memory limit error is reported only when the
matching counter went past its limit."
  (case (getf metrics :error-type)
    (wardlisp/src/types:wardlisp-step-limit-exceeded
     (> (getf metrics :steps-used) fuel))
    (wardlisp/src/types:wardlisp-memory-limit-exceeded
     (> (getf metrics :cons-allocated) max-cons))
    (t t)))

(defun numbers-within-limit-p (value metrics max-integer)
  "Sections 3.1, 3.2 and 11.4: no number in a successful result exceeds
MAX-INTEGER in absolute value."
  (or (getf metrics :error-type)
      (every (lambda (number) (<= (abs number) max-integer))
             (datum-numbers value))))

(defspec-function evaluate
  "Evaluate never signals and never lets a host error through: every outcome,
success or failure, comes back as two values, a failure under one of wardlisp's
own error types, with metrics that account for the resources the run used and
stay inside the limits it was given.  The clauses compare the metrics with the limits the call
passed, so the contract covers calls that pass every limit; the defaults are
not part of it."
  (:args (code string)
         &key ((:fuel fuel) (range integer 1 *))
              ((:max-depth max-depth) (range integer 1 *))
              ((:max-cons max-cons) (range integer 1 *))
              ((:max-output max-output) (range integer 1 *))
              ((:max-integer max-integer) (range integer 1 *))
              ((:random-seed random-seed) (range integer 0 *)))
  (:args-generator evaluate-arguments)
  (:pre fuel max-depth max-cons max-output max-integer)
  (:returns (values t (satisfies metrics-plist-p)))
  (:post-values (value metrics)
    (error-report-consistent-p value metrics)
    (host-errors-contained-p metrics)
    (result-is-wardlisp-value-p value metrics)
    (fuel-accounted-p metrics fuel)
    (output-within-limit-p metrics max-output)
    (success-within-limits-p metrics fuel max-depth max-cons)
    (limit-error-witnessed-p metrics fuel max-cons)
    (numbers-within-limit-p value metrics max-integer)))

;;; --- print-value (sections 10, 17.3) ---

(defun reads-back-as-p (text value)
  "True when the wardlisp reader reads TEXT back as a string equal to the string
VALUE."
  (let ((read (wardlisp-read text)))
    (and (wstring-p read) (string= (wstring-value read) (wstring-value value)))))

(defun decimal-text-value (text)
  "Return the exact rational the plain decimal TEXT denotes -- an optional minus
sign, one or more ASCII digits, a point, one or more ASCII digits -- or NIL when
TEXT is not of that form."
  (let* ((negative (and (plusp (length text)) (char= #\- (char text 0))))
         (body (if negative (subseq text 1) text))
         (point (position #\. body)))
    (when (and point
               (plusp point)
               (< (1+ point) (length body))
               (every (lambda (ch) (char<= #\0 ch #\9)) (remove #\. body :count 1)))
      (let ((digits (subseq body (1+ point))))
        (* (if negative -1 1)
           (+ (parse-integer body :end point)
              (/ (parse-integer digits) (expt 10 (length digits)))))))))

(defun denotes-float-p (text value)
  "Section 10: TEXT is a plain decimal fraction, with at least one digit after
the point and no exponent, whose exact value rounds to the double-float VALUE,
and without trailing zeros after the point beyond the one digit kept."
  (let ((exact (decimal-text-value text))
        (fraction (subseq text (1+ (or (position #\. text) -1)))))
    (and exact
         (nearest-double-p exact value)
         (or (= (length fraction) 1)
             (char/= #\0 (char fraction (1- (length fraction))))))))

(defspec-function print-value
  "Every data value prints as the text section 10 and 17.3 give for its type."
  (:args (value printable-datum))
  (:cases
    (:integer
     "An integer prints in decimal."
     (:when (integerp value))
     (:returns string)
     (:post (string= result (format nil "~D" value))))
    (:float
     "A float prints as a decimal fraction with a point and no exponent, denoting
the float's value."
     (:when (floatp value))
     (:returns string)
     (:post (denotes-float-p result value)))
    (:boolean
     "T and NIL print as t and nil."
     (:when (or (eq value t) (null value)))
     (:returns string)
     (:post (string= result (if value "t" "nil"))))
    (:symbol
     "A symbol prints as its bare name."
     (:when (stringp value))
     (:returns string)
     (:post (string= result value)))
    (:string
     "A string prints quoted, with escapes the reader reads back."
     (:when (wstring-p value))
     (:returns string)
     (:post (char= #\" (char result 0))
            (char= #\" (char result (1- (length result))))
            (reads-back-as-p result value)))
    (:list
     "A proper list prints in parentheses."
     (:when (ocons-p value))
     (:returns string)
     (:post (char= #\( (char result 0))
            (char= #\) (char result (1- (length result))))))))

;;; --- Value introspection (section 17.8) ---

(defspec-function make-string-value
  "A CL string becomes a wardlisp string with the same contents and storage of
its own; anything else is a wardlisp type error."
  (:args (text (or string integer null)))
  (:cases
    (:string
     (:when (stringp text))
     (:returns (satisfies string-value-p))
     (:post (string= (wstring-value result) text)
            (not (eq (wstring-value result) text))))
    (:not-a-string
     (:when (not (stringp text)))
     (:signals (type wardlisp-type-error)))))

(defspec-function string-value
  "A wardlisp string gives back a fresh CL string of its contents; any other
value is a wardlisp type error."
  (:args (value wardlisp-datum))
  (:cases
    (:string
     (:when (wstring-p value))
     (:returns string)
     (:post (string= result (wstring-value value))
            (not (eq result (wstring-value value)))))
    (:not-a-string
     (:when (not (wstring-p value)))
     (:signals (type wardlisp-type-error)))))

(defspec-function string-value-p
  "True exactly for boxed strings."
  (:args (value wardlisp-datum))
  (:returns boolean)
  (:post (eq result (wstring-p value))))

(defspec-function symbol-value-p
  "True exactly for symbols, which are bare CL strings."
  (:args (value wardlisp-datum))
  (:returns boolean)
  (:post (eq result (stringp value))))

(defspec-function number-value-p
  "True exactly for numbers, integer or float."
  (:args (value wardlisp-datum))
  (:returns boolean)
  (:post (eq result (numberp value))))
