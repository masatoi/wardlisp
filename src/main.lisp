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

(defun valid-binding-value-p (value)
  "True if VALUE is a value the host may inject via EVALUATE's :bindings."
  (or (numberp value) (eq value t) (null value)
      (stringp value) (wstring-p value) (ocons-p value)))

(defun build-binding-frame (bindings)
  "Validate BINDINGS, an alist of (\"name\" . value), and return an environment
frame. Names are downcased to match reader symbol normalization. Signals
wardlisp-type-error on malformed input."
  (unless (listp bindings)
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
                                  (type-of name))))
        (when (or (string-equal name "t") (string-equal name "nil"))
          (error 'wardlisp-type-error
                 :message (format nil ":bindings: ~a is a reserved name" name)))
        (unless (valid-binding-value-p value)
          (error 'wardlisp-type-error
                 :message (format nil ":bindings: unsupported value for ~a" name)))
        (push (cons (string-downcase name) value) frame)))))

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
host-provided reader flags. Names are downcased; values may be numbers, t, nil,
symbols (bare strings), boxed strings (see MAKE-STRING-VALUE), or lists."
  ;; Validate inputs before creating execution context
  (unless (stringp code)
    (return-from evaluate
      (values nil (make-metrics nil
                   :error-type 'wardlisp-type-error
                   :error-message (format nil "evaluate: expected string, got ~a"
                                          (let ((tp (type-of code)))
                                            (if (consp tp) (car tp) tp)))))))
  (unless (and (integerp fuel) (plusp fuel))
    (return-from evaluate
      (values nil (make-metrics nil
                   :error-type 'wardlisp-type-error
                   :error-message "evaluate: :fuel must be a positive integer"))))
  (unless (and (integerp max-depth) (plusp max-depth))
    (return-from evaluate
      (values nil (make-metrics nil
                   :error-type 'wardlisp-type-error
                   :error-message "evaluate: :max-depth must be a positive integer"))))
  (unless (and (integerp max-cons) (plusp max-cons))
    (return-from evaluate
      (values nil (make-metrics nil
                   :error-type 'wardlisp-type-error
                   :error-message "evaluate: :max-cons must be a positive integer"))))
  (unless (and (integerp max-output) (plusp max-output))
    (return-from evaluate
      (values nil (make-metrics nil
                   :error-type 'wardlisp-type-error
                   :error-message "evaluate: :max-output must be a positive integer"))))
  (unless (and (integerp max-integer) (plusp max-integer))
    (return-from evaluate
      (values nil (make-metrics nil
                   :error-type 'wardlisp-type-error
                   :error-message "evaluate: :max-integer must be a positive integer"))))
  (unless (and (realp timeout) (plusp timeout))
    (return-from evaluate
      (values nil (make-metrics nil
                   :error-type 'wardlisp-type-error
                   :error-message "evaluate: :timeout must be a positive number"))))
  (unless (or (null random-seed)
              (and (integerp random-seed) (not (minusp random-seed))))
    (return-from evaluate
      (values nil (make-metrics nil
                   :error-type 'wardlisp-type-error
                   :error-message
                   "evaluate: :random-seed must be a non-negative integer or NIL"))))
  ;; Context is created after validation, bound outside handler-case
  ;; so error handlers can access metrics and output
  (let ((ctx (make-exec-ctx :fuel fuel :max-depth max-depth :max-cons max-cons
                            :max-output max-output :max-integer max-integer)))
    (flet ((run-evaluate ()
             (handler-case
                 (sb-ext:with-timeout timeout
                   (let* ((program (wardlisp/src/reader:wardlisp-read-program code))
                          (frame (build-binding-frame bindings))
                          (env (make-initial-env))
                          (result (progn (setf (first env) frame)
                                         (eval-program program env ctx))))
                     (values result (make-metrics ctx))))
               (wardlisp-error (e)
                 (values nil
                         (make-metrics ctx :error-type (type-of e)
                                       :error-message (wardlisp-error-message e))))
               (sb-ext:timeout ()
                 (values nil
                         (make-metrics ctx :error-type 'wardlisp-timeout-exceeded
                                       :error-message
                                       (format nil "Evaluation timed out after ~d second~:p"
                                               timeout))))
               (serious-condition (e)
                 (values nil
                         (make-metrics ctx :error-type 'wardlisp-internal-error
                                       :error-message
                                       (format nil "Internal error: ~a" (type-of e))))))))
      ;; When seeded, shadow *random-state* for reproducibility.
      ;; When unseeded, use the process-global *random-state* directly so each
      ;; call advances the shared state — otherwise binding to a copy via
      ;; (make-random-state ...) would make consecutive evaluate calls produce
      ;; the same sequence.
      (if random-seed
          (let ((*random-state* (sb-ext:seed-random-state random-seed)))
            (run-evaluate))
          (run-evaluate)))))

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
                            (print-value value))))
  (wstring-value value))

(defun make-string-value (string)
  "Construct a wardlisp string value from a CL STRING, for host-side injection
\(e.g. via EVALUATE's :bindings)."
  (unless (stringp string)
    (error 'wardlisp-type-error
           :message "make-string-value: expected a CL string"))
  (make-wstring (copy-seq string)))

(defun symbol-value-p (value)
  "True if VALUE is a wardlisp symbol (a bare lowercase string), not a string
literal and not a boolean."
  (and (stringp value) t))

(defun number-value-p (value)
  "True if VALUE is a wardlisp number (integer or float)."
  (numberp value))
