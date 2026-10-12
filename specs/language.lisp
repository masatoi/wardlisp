;;;; specs/language.lisp
;;;;
;;;; Properties: relations between calls that no single-call contract can
;;;; state.  Each names the section of docs/language-spec.md it comes from and
;;;; is (:about ...) the public functions it constrains.

(in-package :wardlisp/specs)

(defun run (code &rest options)
  "Evaluate CODE with OPTIONS; return the result, the metrics and the error type."
  (multiple-value-bind (value metrics) (apply #'evaluate code options)
    (values value metrics (getf metrics :error-type))))

(defun succeeded-p (metrics)
  "True when METRICS records no error."
  (null (getf metrics :error-type)))

;;; --- Host boundary (sections 13.1, 17.9) ---

(defproperty evaluate-reports-invalid-arguments
    ((code (or string integer null))
     (fuel (or (range integer -3 3) string null)))
  "Section 13.1: a non-string program or a fuel that is not a positive integer
is reported as a wardlisp type error in the metrics, never signalled."
  (:about evaluate)
  (:kind :invariant)
  (:trials (:smoke 20 :normal 200))
  (let ((valid (and (stringp code) (integerp fuel) (plusp fuel))))
    (multiple-value-bind (value metrics type)
        (if (null fuel) (run code) (run code :fuel fuel))
      (and (metrics-plist-p metrics)
           (or valid
               (null fuel)
               (and (null value) (eq type 'wardlisp/src/types:wardlisp-type-error)))
           (or (stringp code)
               (and (null value) (eq type 'wardlisp/src/types:wardlisp-type-error)))))))

(defproperty evaluate-default-fuel-is-documented
    ((code deterministic-program-text))
  "Sections 11.1 and 13.1: without :FUEL a run has 1,000,000 steps, so the
steps it used and the fuel it has left add up to that."
  (:about evaluate)
  (:kind :invariant)
  (:trials (:smoke 20 :normal 100))
  (multiple-value-bind (value metrics) (run code)
    (declare (ignore value))
    (= 1000000 (+ (getf metrics :steps-used) (getf metrics :fuel-remaining)))))

(defproperty evaluate-sees-host-bindings
    ((name wardlisp-symbol-name)
     (value (range integer -1000000 1000000)))
  "Section 17.9: a binding injected with :BINDINGS is visible under its
downcased name, and t and nil cannot be bound."
  (:about evaluate)
  (:kind :invariant)
  (:trials (:smoke 20 :normal 200))
  (multiple-value-bind (seen metrics)
      (run name :bindings (list (cons (string-upcase name) value)))
    (and (succeeded-p metrics)
         (eql seen value)
         (every (lambda (reserved)
                  (eq 'wardlisp/src/types:wardlisp-type-error
                      (nth-value 2 (run "1" :bindings (list (cons reserved value))))))
                '("t" "nil" "T" "NIL")))))

(defproperty evaluate-checks-bound-numbers
    ((value (or (range integer -100000 100000) (member 1/3 -5/2 #C(1 2) 2.5f0 1.5d0 -7.25d0)))
     (limit (range integer 1 1000)))
  "Section 17.9: a number injected through :BINDINGS must be an integer or a
double-float (sections 3.1 and 3.2) -- any other number is a type-error -- and
is subject to max-integer like any other number (section 11.4)."
  (:about evaluate)
  (:kind :invariant)
  (:trials (:smoke 20 :normal 200))
  (multiple-value-bind (seen metrics type)
      (run "x" :bindings (list (cons "x" value)) :max-integer limit)
    (declare (ignore metrics))
    (cond ((not (wardlisp-number-p value))
           (eq type 'wardlisp/src/types:wardlisp-type-error))
          ((> (abs value) limit)
           (eq type 'wardlisp/src/types:wardlisp-integer-limit-exceeded))
          (t (and (null type) (eql seen value))))))

(defproperty evaluate-is-deterministic
    ((code program-text)
     (seed (range integer 0 1000)))
  "Section 1 (determinism) and 7.4 (random): the same program under the same
limits and :RANDOM-SEED gives the same result and the same metrics, programs
that call random included."
  (:about evaluate)
  (:kind :invariant)
  (:trials (:smoke 20 :normal 200))
  (multiple-value-bind (first first-metrics) (run code :fuel 5000 :random-seed seed)
    (multiple-value-bind (second second-metrics) (run code :fuel 5000 :random-seed seed)
      (and (equal (print-value first) (print-value second))
           (equal first-metrics second-metrics)))))

(defproperty seeded-random-is-reproducible
    ((bound (range integer 1 1000000))
     (seed (range integer 0 100000)))
  "Sections 7.4 and 13.1: (random N) gives an integer from 0 below N, and the
same :RANDOM-SEED gives the same sequence in every evaluation."
  (:about evaluate)
  (:kind :invariant)
  (:trials (:smoke 20 :normal 200))
  (let ((code (format nil "(list ~{(random ~D)~^ ~})" (make-list 6 :initial-element bound))))
    (multiple-value-bind (first metrics) (run code :random-seed seed)
      (and (succeeded-p metrics)
           (equal (print-value first) (print-value (run code :random-seed seed)))
           (every (lambda (n) (and (integerp n) (<= 0 n) (< n bound)))
                  (ocons-elements first))))))

(defparameter *hostile-host-traps* '(:overflow :invalid :divide-by-zero :underflow)
  "Float traps unlike the ones wardlisp runs under: underflow traps too.")

(defproperty evaluate-ignores-host-dynamic-environment
    ((code program-text)
     (seed (range integer 0 1000)))
  "Sections 1 and 2: what a program means does not depend on the host's reader
variables, readtable or floating-point traps.  Under unusual host settings it
gives the same result and metrics as under standard ones, and the host's float
traps are as they were afterwards."
  (:about evaluate)
  (:kind :invariant)
  (:trials (:smoke 20 :normal 200))
  (multiple-value-bind (expected expected-metrics) (run code :fuel 5000 :random-seed seed)
    (let ((saved (sb-int:get-floating-point-modes)))
      (unwind-protect
           (progn
             (sb-int:set-floating-point-modes :traps *hostile-host-traps*)
             (multiple-value-bind (actual actual-metrics)
                 (let ((*read-base* 16)
                       (*read-default-float-format* 'single-float)
                       (*print-base* 16)
                       (*print-radix* t)
                       (*print-case* :downcase)
                       (*readtable* (copy-readtable nil)))
                   (set-macro-character #\1 (lambda (stream char)
                                              (declare (ignore stream char))
                                              :host-macro))
                   (run code :fuel 5000 :random-seed seed))
               (and (equal (print-value actual) (print-value expected))
                    (equal actual-metrics expected-metrics)
                    (null (set-exclusive-or
                           (getf (sb-int:get-floating-point-modes) :traps)
                           *hostile-host-traps*)))))
        (apply #'sb-int:set-floating-point-modes saved)))))

;;; --- Resource limits (section 11) ---

(defun limit-is-tight-p (code metrics-key option error-type)
  "Run CODE with generous fuel and default limits, read the amount of
METRICS-KEY it used, and check section 11's rule for the limit OPTION: a run
with exactly that limit behaves as the generous one, and one less stops it with
ERROR-TYPE.  CODE comes from *LIMIT-PROGRAMS*, so the generous run must succeed
and use at least two units; anything else fails the check rather than passing
it vacuously."
  (multiple-value-bind (value metrics) (run code :fuel 20000)
    (let ((used (if (eq metrics-key :output)
                    (length (getf metrics :output))
                    (getf metrics metrics-key))))
      (and (succeeded-p metrics)
           (>= used 2)
           (multiple-value-bind (exact exact-metrics) (run code :fuel 20000 option used)
             (and (succeeded-p exact-metrics)
                  (equal (print-value exact) (print-value value))
                  (eq error-type
                      (nth-value 2 (run code :fuel 20000 option (1- used))))))))))

(defproperty step-limit-is-exact
    ((code step-using-program))
  "Section 11.1: a program that needs N steps runs with fuel N, using it up
exactly, and stops with step-limit-exceeded with fuel N - 1."
  (:about evaluate)
  (:kind :invariant)
  (:trials (:smoke 20 :normal 100))
  (multiple-value-bind (value metrics) (run code :fuel 20000)
    (let ((steps (getf metrics :steps-used)))
      (and (succeeded-p metrics)
           (>= steps 2)
           (multiple-value-bind (exact exact-metrics) (run code :fuel steps)
             (and (succeeded-p exact-metrics)
                  (zerop (getf exact-metrics :fuel-remaining))
                  (equal (print-value exact) (print-value value))
                  (eq 'wardlisp/src/types:wardlisp-step-limit-exceeded
                      (nth-value 2 (run code :fuel (1- steps))))))))))

(defproperty memory-limit-is-exact
    ((code cons-using-program))
  "Section 11.3: a program that allocates N cells runs with max-cons N and
stops with memory-limit-exceeded with max-cons N - 1."
  (:about evaluate)
  (:kind :invariant)
  (:trials (:smoke 20 :normal 100))
  (limit-is-tight-p code :cons-allocated :max-cons
                    'wardlisp/src/types:wardlisp-memory-limit-exceeded))

(defproperty recursion-limit-is-exact
    ((code depth-using-program))
  "Section 11.2: a program that reaches call depth N runs with max-depth N and
stops with recursion-limit-exceeded with max-depth N - 1."
  (:about evaluate)
  (:kind :invariant)
  (:trials (:smoke 20 :normal 100))
  (limit-is-tight-p code :max-depth-reached :max-depth
                    'wardlisp/src/types:wardlisp-recursion-limit-exceeded))

(defproperty output-limit-is-exact
    ((code output-using-program))
  "Section 11.5: a program that prints N characters runs with max-output N and
stops with output-limit-exceeded with max-output N - 1."
  (:about evaluate)
  (:kind :invariant)
  (:trials (:smoke 20 :normal 100))
  (limit-is-tight-p code :output :max-output
                    'wardlisp/src/types:wardlisp-output-limit-exceeded))

;;; --- Arithmetic (sections 7.1, 11.4) ---

(defproperty integer-arithmetic-agrees-with-common-lisp
    ((a wide-integer)
     (b wide-integer)
     (operator (member :add :subtract :multiply)))
  "Sections 3.1, 7.1 and 11.4: +, - and * on integers give the exact integer
result while the operands and the result are at most max-integer in absolute
value, and integer-limit-exceeded otherwise."
  (:about evaluate)
  (:kind :model)
  (:trials (:smoke 30 :normal 300))
  (let ((limit (expt 2 64))
        (name (ecase operator (:add "+") (:subtract "-") (:multiply "*")))
        (expected (funcall (ecase operator (:add #'+) (:subtract #'-) (:multiply #'*))
                           a b)))
    (multiple-value-bind (value metrics type)
        (run (format nil "(~A ~D ~D)" name a b) :max-integer limit)
      (declare (ignore metrics))
      (if (and (<= (abs a) limit) (<= (abs b) limit) (<= (abs expected) limit))
          (eql value expected)
          (eq type 'wardlisp/src/types:wardlisp-integer-limit-exceeded)))))

(defproperty out-of-range-float-literals-are-parse-errors
    ((literal out-of-range-float-literal)
     (context (member :alone :quoted :argument)))
  "Sections 2.5.2 and 12: a float literal beyond double-float range is a
parse-error wherever it appears, however large max-integer is."
  (:about evaluate)
  (:kind :invariant)
  (:trials (:smoke 20 :normal 100))
  (let ((code (ecase context
                (:alone literal)
                (:quoted (format nil "'(1 ~A)" literal))
                (:argument (format nil "(+ 1 ~A)" literal)))))
    (eq 'wardlisp/src/types:wardlisp-parse-error
        (nth-value 2 (run code :max-integer (expt 10 1000))))))

(defproperty non-ascii-digits-are-symbol-characters
    ((token non-ascii-number-token))
  "Sections 2.6 and 15: only the ASCII digits 0-9 are digits, so a token made of
other Unicode decimal digits is a symbol: unquoted it is an undefined name,
quoted it evaluates to that symbol."
  (:about evaluate)
  (:kind :invariant)
  (:trials (:smoke 20 :normal 100))
  (multiple-value-bind (quoted metrics) (run (format nil "'~A" token))
    (and (succeeded-p metrics)
         (symbol-value-p quoted)
         (string= quoted (string-downcase token))
         (eq 'wardlisp/src/types:wardlisp-name-error (nth-value 2 (run token))))))

(defproperty float-overflow-is-a-number-limit
    ((left wide-float-literal)
     (right wide-float-literal)
     (operator (member :add :subtract :multiply :divide)))
  "Section 11.4: when max-integer is beyond double-float range, float arithmetic
gives the double-float result while the exact result is well inside that range,
and integer-limit-exceeded once it is well outside; near the boundary either is
allowed, and nothing else is ever reported."
  (:about evaluate)
  (:kind :model)
  (:trials (:smoke 30 :normal 300))
  (let* ((a (wardlisp-read left))
         (b (wardlisp-read right))
         (function (ecase operator (:add #'+) (:subtract #'-) (:multiply #'*) (:divide #'/)))
         (exact (abs (funcall function (rational a) (rational b))))
         (bound (rational most-positive-double-float)))
    (multiple-value-bind (value metrics type)
        (run (format nil "(~A ~A ~A)"
                     (ecase operator (:add "+") (:subtract "-") (:multiply "*") (:divide "/"))
                     left right)
             :max-integer (expt 10 1000))
      (declare (ignore metrics))
      (cond ((< exact (/ bound 2))
             (and (null type) (eql value (funcall function a b))))
            ((> exact (* bound 2))
             (eq type 'wardlisp/src/types:wardlisp-integer-limit-exceeded))
            (t
             (member type '(nil wardlisp/src/types:wardlisp-integer-limit-exceeded)))))))

(defproperty quotient-and-mod-truncate-toward-zero
    ((a (range integer -1000000 1000000))
     (b (range integer -1000 1000)))
  "Section 7.1: a = (quotient a b) * b + (mod a b), the remainder is smaller
than the divisor and has the dividend's sign, and dividing by zero is a type
error."
  (:about evaluate)
  (:kind :model)
  (:trials (:smoke 30 :normal 300))
  (let ((q (run (format nil "(quotient ~D ~D)" a b)))
        (m (run (format nil "(mod ~D ~D)" a b))))
    (if (zerop b)
        (and (eq 'wardlisp/src/types:wardlisp-type-error
                 (nth-value 2 (run (format nil "(quotient ~D 0)" a))))
             (eq 'wardlisp/src/types:wardlisp-type-error
                 (nth-value 2 (run (format nil "(mod ~D 0)" a)))))
        (and (integerp q) (integerp m)
             (= a (+ (* q b) m))
             (< (abs m) (abs b))
             (or (zerop m) (eq (minusp m) (minusp a)))))))

;;; --- Data, printing and equality (sections 7.4, 10, 17) ---

(defproperty quoted-data-round-trip-through-print-value
    ((value wardlisp-datum))
  "Sections 10 and 17.3: quoting the printed form of a data value and
evaluating it gives a value that prints the same."
  (:about print-value evaluate)
  (:kind :round-trip)
  (:trials (:smoke 30 :normal 300))
  (multiple-value-bind (read metrics) (run (datum-source value))
    (and (succeeded-p metrics)
         (string= (print-value read) (print-value value)))))

(defproperty equal?-is-structural-equality
    ((pair exact-datum-pair))
  "Sections 7.4 and 17.4: over data without floats, equal? holds exactly when
the two values print the same; strings and symbols never compare equal."
  (:about evaluate print-value)
  (:kind :model)
  (:trials (:smoke 30 :normal 300))
  (destructuring-bind (left right) pair
    (multiple-value-bind (verdict metrics)
        (run (format nil "(equal? ~A ~A)" (datum-source left) (datum-source right)))
      (and (succeeded-p metrics)
           (eq (eq verdict t)
               (string= (print-value left) (print-value right)))))))

(defproperty value-classifiers-partition-data
    ((value wardlisp-datum))
  "Section 17.8: every data value is exactly one of a number, a symbol, a
string, a list cell or one of t and nil."
  (:about string-value-p symbol-value-p number-value-p ocons-p)
  (:kind :invariant)
  (:trials (:smoke 30 :normal 300))
  (= 1 (count t (list (number-value-p value)
                      (symbol-value-p value)
                      (string-value-p value)
                      (ocons-p value)
                      (or (eq value t) (null value))))))

(defproperty host-strings-are-copied-both-ways
    ((text host-string))
  "Section 17.8: a wardlisp string shares no storage with the host string it
was made from or the one STRING-VALUE returns, so mutating either leaves it
unchanged."
  (:about make-string-value string-value)
  (:kind :invariant)
  (:trials (:smoke 20 :normal 200))
  (let ((original (copy-seq text))
        (boxed (make-string-value text)))
    (fill text #\x)
    (let ((out (string-value boxed)))
      (fill out #\y)
      (string= (string-value boxed) original))))

(defproperty evaluate-results-carry-host-strings
    ((text host-string))
  "Section 17.9: a string injected through :BINDINGS comes back from EVALUATE as
a string with the same contents."
  (:about evaluate make-string-value string-value)
  (:kind :round-trip)
  (:trials (:smoke 20 :normal 200))
  (multiple-value-bind (value metrics)
      (run "(list greeting)" :bindings (list (cons "greeting" (make-string-value text))))
    (and (succeeded-p metrics)
         (ocons-p value)
         (string-value-p (ocons-ocar value))
         (string= (string-value (ocons-ocar value)) text))))
