(defpackage :wardlisp
  (:use :cl
        :wardlisp/src/types
        :wardlisp/src/evaluator
        :wardlisp/src/builtins)
  (:export #:evaluate #:print-value
           ;; Value introspection for host result-walking (R3)
           #:ocons-p #:ocons-ocar #:ocons-ocdr
           #:string-value-p #:string-value #:make-string-value
           #:symbol-value-p #:number-value-p
           #:wardlisp-error #:wardlisp-error-message
           #:wardlisp-parse-error #:wardlisp-name-error
           #:wardlisp-type-error #:wardlisp-arity-error
           #:wardlisp-step-limit-exceeded
           #:wardlisp-recursion-limit-exceeded
           #:wardlisp-memory-limit-exceeded
           #:wardlisp-integer-limit-exceeded
           #:wardlisp-output-limit-exceeded
           #:wardlisp-timeout-exceeded))
(in-package :wardlisp)

(defun finite-float-p (value)
  "True if VALUE is a float that is neither an infinity nor a NaN."
  (and (floatp value)
       (not (sb-ext:float-infinity-p value))
       (not (sb-ext:float-nan-p value))))

(defun proper-list-p (value)
  "True if VALUE is a finite, NIL-terminated list: not dotted, not circular."
  (and (listp value)
       (handler-case (list-length value)
         (type-error () nil))
       t))

(defconstant +max-timeout+ 1000000
  "The largest :timeout EVALUATE accepts, in seconds (about eleven days).  The
host's timer arithmetic overflows somewhere above 1e18 seconds and is then
broken for every later call; this cap keeps far away from that and from any
timeout an evaluation could need.")

(defun valid-timeout-p (timeout)
  "True if TIMEOUT is a real number of seconds greater than zero and at most
+MAX-TIMEOUT+.  A NaN is tested before any comparison, which it would trap."
  (and (realp timeout)
       (not (and (floatp timeout) (sb-ext:float-nan-p timeout)))
       (< 0 timeout)
       (<= timeout +max-timeout+)))

(defun valid-binding-value-p (value)
  "True if VALUE is a value the host may inject via EVALUATE's :bindings."
  ;; Atoms only: integer, finite float (converted to double-float on the way
  ;; in), t, nil, symbol (CL string), or boxed string. Other CL numbers (ratios,
  ;; complexes) are not wardlisp values, and compound ocons values are rejected
  ;; -- the public API exports no ocons constructor and their contents would be
  ;; unvalidated host objects.
  (or (integerp value) (finite-float-p value) (eq value t) (null value)
      (stringp value) (wstring-p value)))

(defun build-binding-frame (bindings ctx)
  "Validate BINDINGS, an alist of (\"name\" . value), and return an environment
frame. Names are downcased to match reader symbol normalization. Signals
wardlisp-type-error on malformed input, and wardlisp-integer-limit-exceeded for
a number beyond CTX's max-integer."
  (unless (proper-list-p bindings)
    (error 'wardlisp-type-error :message "evaluate: :bindings must be an alist"))
  (let ((frame nil))
    (dolist (b bindings (nreverse frame))
      (unless (consp b)
        (error 'wardlisp-type-error
               :message ":bindings: each entry must be a (name . value) pair"))
      (let ((name (car b))
            (value (cdr b)))
        (unless (stringp name)
          (error 'wardlisp-type-error
                 :message (format nil ":bindings: name must be a string, got ~a"
                                  (let ((type (type-of name)))
                                    (if (consp type) (car type) type)))))
        (when (or (string-equal name "t") (string-equal name "nil"))
          (error 'wardlisp-type-error
                 :message (format nil ":bindings: ~a is a reserved name" name)))
        (unless (valid-binding-value-p value)
          (error 'wardlisp-type-error
                 :message (format nil ":bindings: unsupported value for ~a"
                                  (abbreviate-text name))))
        (when (floatp value)
          (setf value (coerce value 'double-float)))
        (when (numberp value)
          (check-integer ctx value))
        (push (cons (string-downcase name) value) frame)))))

(defun call-with-sandbox-host-environment (thunk)
  "Call THUNK in the host environment evaluation relies on, whatever the caller
has set.  Reader and printer variables take their standard values (with
*PRINT-READABLY* false), so neither the meaning of a literal nor the text of an
error message depends on the caller's *READ-BASE*, readtable or *PRINT-BASE*,
and the caller's *BREAK-ON-SIGNALS* does not apply.
Floats round to nearest, and subnormals are neither flushed to zero nor read as
zero, whatever flush-to-zero or denormals-are-zero bits the host has set.
Overflow, invalid operation and division by zero trap, so they can be
classified as wardlisp errors; underflow and inexact do not, so a result too
small to represent rounds to zero.  The caller's float modes, every bit of
them, are restored afterwards."
  (let ((saved (sb-vm:floating-point-modes)))
    ;; Setting and restoring the modes run with interrupts deferred, so an
    ;; interrupt that unwinds cannot leave the host with the sandbox's modes.
    (sb-sys:without-interrupts
      (unwind-protect
           (progn
             (sb-int:set-floating-point-modes :traps '(:overflow :invalid :divide-by-zero)
                                              :rounding-mode :nearest)
             ;; MXCSR bit 15 flushes subnormal results to zero and bit 6 reads
             ;; subnormal operands as zero; SET-FLOATING-POINT-MODES leaves them.
             #+x86-64
             (setf (sb-vm:floating-point-modes)
                   (logandc2 (sb-vm:floating-point-modes) (logior (ash 1 15) (ash 1 6))))
             (sb-sys:with-local-interrupts
               (with-standard-io-syntax
                 ;; Errors are signalled and classified inside evaluation; the
                 ;; caller's *BREAK-ON-SIGNALS* must not turn them into breaks.
                 (let ((*print-readably* nil)
                       (*break-on-signals* nil))
                   (funcall thunk)))))
        (setf (sb-vm:floating-point-modes) saved)))))

(defun invalid-argument (message)
  "Return EVALUATE's two values for an argument refused with MESSAGE."
  (values nil (make-metrics nil :error-type 'wardlisp-type-error :error-message message)))

(defun host-signal-p (condition)
  "True if CONDITION is one the host raises asynchronously -- a timer's timeout
or an interactive interrupt -- rather than one evaluation caused.  EVALUATE
lets such a condition through unless it is its own timeout."
  (typep condition '(or sb-ext:timeout sb-sys:interactive-interrupt)))

(define-condition sandbox-deadline-reached (serious-condition) ()
  (:documentation "Signalled by EVALUATE's own timer when :timeout expires.  A
condition of its own, so that EVALUATE never mistakes a timeout the host set
around it for its own, nor its own for the host's."))

(defun call-with-sandbox-deadline (seconds ctx thunk)
  "Call THUNK under a deadline of SECONDS, in two stages.  When SECONDS pass, a
timer marks CTX's deadline as passed and returns normally; the evaluator stops
at its next step with wardlisp-timeout-exceeded.  Returning normally matters:
a timer that unwinds can make SBCL drop another timer expiring at the same
moment, such as one the host set around EVALUATE.  Should a single step run on
for another second, a second timer signals SANDBOX-DEADLINE-REACHED to stop it."
  (let ((soft (sb-ext:make-timer (lambda () (setf (exec-ctx-deadline-passed ctx) t))))
        (hard (sb-ext:make-timer (lambda () (error 'sandbox-deadline-reached)))))
    ;; Scheduling and the cleanup run with interrupts deferred, and only THUNK
    ;; with them enabled: an interrupt (a host timeout, say) that unwinds
    ;; between the two SCHEDULE-TIMER calls or in the middle of the cleanup
    ;; would otherwise leave the hard timer to fire later in the host's own
    ;; code, or inside another evaluation.
    (sb-sys:without-interrupts
      (unwind-protect
           (progn
             (sb-ext:schedule-timer soft seconds)
             (sb-ext:schedule-timer hard (+ seconds 1))
             (sb-sys:with-local-interrupts (funcall thunk)))
        (sb-ext:unschedule-timer hard)
        (sb-ext:unschedule-timer soft)))))

(defun timeout-message (timeout)
  "Return the error message for an evaluation stopped after TIMEOUT seconds."
  (cond ((integerp timeout)
         (format nil "Evaluation timed out after ~d second~:p" timeout))
        ((< timeout 1/1000)
         "Evaluation timed out after less than 0.001 seconds")
        (t (format nil "Evaluation timed out after ~a seconds"
                   (describe-value (float timeout 1d0))))))

(defun evaluate-in-sandbox (code fuel max-depth max-cons max-output max-integer timeout
                            random-seed bindings)
  "Do EVALUATE's work, inside CALL-WITH-SANDBOX-HOST-ENVIRONMENT."
  ;; Validate inputs before creating execution context
  (unless (stringp code)
    (return-from evaluate-in-sandbox
      (invalid-argument (format nil "evaluate: expected string, got ~a"
                                (let ((tp (type-of code)))
                                  (if (consp tp) (car tp) tp))))))
  (unless (and (integerp fuel) (plusp fuel))
    (return-from evaluate-in-sandbox
      (invalid-argument "evaluate: :fuel must be a positive integer")))
  (unless (and (integerp max-depth) (plusp max-depth))
    (return-from evaluate-in-sandbox
      (invalid-argument "evaluate: :max-depth must be a positive integer")))
  (unless (and (integerp max-cons) (plusp max-cons))
    (return-from evaluate-in-sandbox
      (invalid-argument "evaluate: :max-cons must be a positive integer")))
  (unless (and (integerp max-output) (plusp max-output))
    (return-from evaluate-in-sandbox
      (invalid-argument "evaluate: :max-output must be a positive integer")))
  (unless (and (integerp max-integer) (plusp max-integer))
    (return-from evaluate-in-sandbox
      (invalid-argument "evaluate: :max-integer must be a positive integer")))
  (unless (valid-timeout-p timeout)
    (return-from evaluate-in-sandbox
      (invalid-argument (format nil "evaluate: :timeout must be a positive number of ~
                                     at most ~d seconds" +max-timeout+))))
  (unless (or (null random-seed)
              (and (integerp random-seed) (not (minusp random-seed))))
    (return-from evaluate-in-sandbox
      (invalid-argument "evaluate: :random-seed must be a non-negative integer or NIL")))
  ;; Context is created after validation, bound outside handler-case
  ;; so error handlers can access metrics and output
  (let ((ctx (make-exec-ctx :fuel fuel :max-depth max-depth :max-cons max-cons
                            :max-output max-output :max-integer max-integer)))
    (flet ((run-evaluate ()
             ;; The handlers only classify and unwind: they run where the
             ;; condition was signalled, possibly on an exhausted stack, so the
             ;; metrics are built after unwinding.
             (multiple-value-bind (outcome payload)
                 (block evaluation
                     (handler-bind
                         ((wardlisp-error
                            (lambda (e) (return-from evaluation (values :wardlisp-error e))))
                          (sandbox-deadline-reached
                            (lambda (e)
                              (declare (ignore e))
                              (return-from evaluation (values :timeout nil))))
                          (serious-condition
                            ;; A timeout or interrupt the host set around
                            ;; EVALUATE is declined, so it reaches the host.
                            (lambda (e)
                              (unless (host-signal-p e)
                                (return-from evaluation (values :internal-error e))))))
                       (call-with-sandbox-deadline
                        timeout ctx
                        (lambda ()
                         ;; Host arguments are checked before the program is read,
                         ;; so a malformed :bindings is reported whatever the
                         ;; program text.
                         ;; LET evaluates its forms left to right: the frame first.
                         (let ((frame (build-binding-frame bindings ctx))
                               (program (wardlisp/src/reader:wardlisp-read-program code))
                               (env (make-initial-env)))
                           (setf (first env) frame)
                           (values :value (eval-program program env ctx)))))))
               (ecase outcome
                 (:value (values payload (make-metrics ctx)))
                 (:wardlisp-error
                  (values nil (make-metrics ctx :error-type (type-of payload)
                                                :error-message
                                                (if (typep payload 'wardlisp-timeout-exceeded)
                                                    (timeout-message timeout)
                                                    (wardlisp-error-message payload)))))
                 (:timeout
                  (values nil (make-metrics ctx :error-type 'wardlisp-timeout-exceeded
                                                :error-message (timeout-message timeout))))
                 (:internal-error
                  (values nil (make-metrics ctx :error-type 'wardlisp-internal-error
                                                :error-message
                                                (format nil "Internal error: ~a"
                                                        (type-of payload)))))))))
      ;; When seeded, shadow *random-state* for reproducibility.
      ;; When unseeded, use the process-global *random-state* directly so each
      ;; call advances the shared state — otherwise binding to a copy via
      ;; (make-random-state ...) would make consecutive evaluate calls produce
      ;; the same sequence.
      (if random-seed
          (let ((*random-state* (sb-ext:seed-random-state random-seed)))
            (run-evaluate))
          (run-evaluate)))))

(defun evaluate (code &key (fuel 1000000) (max-depth 100) (max-cons 10000)
                      (max-output 1000) (max-integer (expt 2 64)) (timeout 5)
                      (random-seed nil) (bindings nil))
  "Evaluate CODE string in the restricted Lisp.
Returns (values result metrics-plist).
On error, returns (values nil metrics-plist) with :error-type and :error-message.
Note: NIL is a valid successful result.  Always check :error-type to distinguish
success from failure.

If RANDOM-SEED is a non-negative integer, calls to (random N) within this
evaluation produce a deterministic sequence reproducible across calls.
When RANDOM-SEED is NIL (default), the process-global *random-state* is used
directly so each call advances the shared state and consecutive evaluations
produce independent random sequences.

BINDINGS, when supplied, is an alist of (\"name\" . value) entries injected into
the initial environment before evaluation, so scene code can reference
host-provided reader flags. Names are downcased; values may be integers and
finite floats (converted to double-floats), both within MAX-INTEGER, t, nil,
symbols (bare strings), or boxed strings (see MAKE-STRING-VALUE).

The result does not depend on the caller's reader and printer variables or
floating-point traps (see CALL-WITH-SANDBOX-HOST-ENVIRONMENT)."
  ;; Last line of defence: EVALUATE never signals.  A condition that escapes
  ;; everything inside is a defect, reported as internal-error -- except a
  ;; timeout or interrupt the host raised itself, which is the host's own.
  (let ((escaped
          (block guarded
            (handler-bind
                ((serious-condition
                   (lambda (e)
                     (unless (host-signal-p e)
                       (return-from guarded e)))))
              (return-from evaluate
                (call-with-sandbox-host-environment
                 (lambda ()
                   (evaluate-in-sandbox code fuel max-depth max-cons max-output max-integer
                                        timeout random-seed bindings))))))))
    (values nil (make-metrics nil :error-type 'wardlisp-internal-error
                                  :error-message (format nil "Internal error: ~a"
                                                         (type-of escaped))))))

(defun make-metrics (ctx &key error-type error-message)
  "Build metrics plist from execution context.  CTX may be nil if context creation itself failed."
  (if ctx
      (list :steps-used (exec-ctx-steps-used ctx)
            :max-depth-reached (exec-ctx-max-depth-reached ctx)
            :cons-allocated (exec-ctx-cons-count ctx)
            :output (copy-seq (exec-ctx-output ctx))
            :fuel-remaining (exec-ctx-fuel ctx)
            :error-type error-type
            :error-message error-message)
      (list :steps-used 0 :max-depth-reached 0 :cons-allocated 0
            :output "" :fuel-remaining 0
            :error-type error-type :error-message error-message)))

;;; --- Public value introspection (host result-walking, R3) ---
;;; These let the host classify and take apart an EVALUATE result using only
;;; the :wardlisp package, with no dependency on internal source packages.

(defun string-value-p (value)
  "True if VALUE is a wardlisp string (boxed), as opposed to a symbol."
  (wstring-p value))

(defun string-value (value)
  "Return the underlying CL string of a wardlisp string VALUE.
Signals a type error if VALUE is not a string."
  (unless (wstring-p value)
    (error 'wardlisp-type-error
           :message (format nil "string-value: not a string: ~a"
                            (describe-value value))))
  ;; Copy on egress (symmetric with make-string-value's ingress copy) so a host
  ;; mutating the returned string cannot corrupt the shared wstring.
  (copy-seq (wstring-value value)))

(defun make-string-value (string)
  "Construct a wardlisp string value from a CL STRING, for host-side injection
\(e.g. via EVALUATE's :bindings)."
  (unless (stringp string)
    (error 'wardlisp-type-error
           :message "make-string-value: expected a CL string"))
  (make-wstring (copy-seq string)))

(defun symbol-value-p (value)
  "True if VALUE is a wardlisp symbol. Symbols are represented as bare CL
strings, so this is true for any CL string and false for boxed strings (see
STRING-VALUE-P) and booleans."
  (and (stringp value) t))

(defun number-value-p (value)
  "True if VALUE is a wardlisp number (integer or float)."
  (numberp value))
