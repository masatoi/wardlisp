(defpackage :wardlisp/src/builtins
  (:use :cl :wardlisp/src/types :wardlisp/src/env)
  (:export #:make-initial-env #:print-value #:print-value-limited
           #:describe-value #:describe-form))
(in-package :wardlisp/src/builtins)

(defun make-initial-env ()
  "Create the initial environment with all built-in functions."
  (let ((builtins
          (list
           ;; Arithmetic (variadic: +, -, *)
           (cons "+" (make-builtin "+" #'builtin-add nil))
           (cons "-" (make-builtin "-" #'builtin-sub nil))
           (cons "*" (make-builtin "*" #'builtin-mul nil))
           (cons "quotient" (make-builtin "quotient" #'builtin-quotient 2))
           (cons "mod" (make-builtin "mod" #'builtin-mod 2))
           (cons "/" (make-builtin "/" #'builtin-fdiv 2))
           ;; Comparison (all arity 2)
           (cons "=" (make-builtin "=" #'builtin-num-eq 2))
           (cons "<" (make-builtin "<" #'builtin-lt 2))
           (cons "<=" (make-builtin "<=" #'builtin-le 2))
           (cons ">" (make-builtin ">" #'builtin-gt 2))
           (cons ">=" (make-builtin ">=" #'builtin-ge 2))
           ;; List operations
           (cons "cons" (make-builtin "cons" #'builtin-cons 2))
           (cons "car" (make-builtin "car" #'builtin-car 1))
           (cons "cdr" (make-builtin "cdr" #'builtin-cdr 1))
           (cons "list" (make-builtin "list" #'builtin-list nil))
           (cons "null?" (make-builtin "null?" #'builtin-null-p 1))
           (cons "atom?" (make-builtin "atom?" #'builtin-atom-p 1))
           (cons "length" (make-builtin "length" #'builtin-length 1))
           (cons "append" (make-builtin "append" #'builtin-append nil))
           ;; Other
           (cons "not" (make-builtin "not" #'builtin-not 1))
           (cons "eq?" (make-builtin "eq?" #'builtin-eq-p 2))
           (cons "equal?" (make-builtin "equal?" #'builtin-equal-p 2))
           (cons "integer?" (make-builtin "integer?" #'builtin-integer-p 1))
           (cons "number?" (make-builtin "number?" #'builtin-number-p 1))
           (cons "print" (make-builtin "print" #'builtin-print 1))
           (cons "random" (make-builtin "random" #'builtin-random 1))
           ;; String operations
           (cons "string-append"
                 (make-builtin "string-append" #'builtin-string-append nil))
           (cons "number->string"
                 (make-builtin "number->string" #'builtin-number->string 1))
           (cons "string-length"
                 (make-builtin "string-length" #'builtin-string-length 1)))))
    (list nil builtins)))

;;; --- Helpers ---

(defun ensure-integer (name value)
  "Ensure VALUE is an integer, or signal a type error."
  (unless (integerp value)
    (error 'wardlisp-type-error
           :message (format nil "~a: expected integer, got ~a" name (describe-value value)))))

(defun ensure-number (name value)
  "Ensure VALUE is a number (integer or float), or signal a type error."
  (unless (numberp value)
    (error 'wardlisp-type-error
           :message (format nil "~a: expected number, got ~a"
                            name (describe-value value)))))

(defun ensure-ocons (name value)
  "Ensure VALUE is an ocons pair, or signal a type error."
  (unless (ocons-p value)
    (error 'wardlisp-type-error
           :message (format nil "~a: expected pair, got ~a" name (describe-value value)))))

(defun ensure-wstring (name value)
  "Ensure VALUE is a string, or signal a type error."
  (unless (wstring-p value)
    (error 'wardlisp-type-error
           :message (format nil "~a: expected string, got ~a" name (describe-value value)))))

;;; --- Arithmetic ---

(defun call-checking-float-range (name thunk)
  "Call THUNK, which computes the result of the arithmetic builtin NAME.  A float
overflow -- a result, or an integer converted to a float, beyond double-float
range -- is reported as wardlisp-integer-limit-exceeded: such a number exceeds
every size the language can represent, whatever max-integer allows, and the host
condition must not escape the sandbox."
  (handler-case (funcall thunk)
    (floating-point-overflow ()
      (error 'wardlisp-integer-limit-exceeded
             :message (format nil "~a: result exceeds the float range" name)))))

(defun checked-reduce (ctx function args initial)
  "Reduce ARGS with FUNCTION from INITIAL, checking every partial result against
CTX's max-integer, so a long argument list never builds a number far beyond the
limit within one step."
  (let ((result initial))
    (dolist (a args (check-integer ctx result))
      (setf result (check-integer ctx (funcall function result a))))))

(defun builtin-add (args ctx)
  "Built-in + function. Variadic addition.  Each partial sum is checked against
max-integer."
  (dolist (a args) (ensure-number "+" a))
  (call-checking-float-range
   "+" (lambda () (checked-reduce ctx #'+ args 0))))

(defun builtin-sub (args ctx)
  "Built-in - function. Unary negation or variadic subtraction.  Each partial
difference is checked against max-integer."
  (when (null args)
    (error 'wardlisp-arity-error :message "- requires at least 1 argument"))
  (dolist (a args) (ensure-number "-" a))
  (call-checking-float-range
   "-" (lambda ()
         (if (= 1 (length args))
             (check-integer ctx (- (first args)))
             (checked-reduce ctx #'- (rest args) (first args))))))

(defun builtin-mul (args ctx)
  "Built-in * function. Variadic multiplication.  Each partial product is
checked against max-integer, so (apply * long-list) never multiplies numbers
far beyond the limit."
  (dolist (a args) (ensure-number "*" a))
  (call-checking-float-range
   "*" (lambda () (checked-reduce ctx #'* args 1))))

(defun builtin-quotient (args ctx)
  "Built-in quotient function. Integer division (truncate toward zero)."
  (dolist (a args) (ensure-integer "quotient" a))
  (when (zerop (second args))
    (error 'wardlisp-type-error :message "quotient: division by zero"))
  (check-integer ctx (truncate (first args) (second args))))

(defun builtin-mod (args ctx)
  "Built-in mod function. Integer modulo."
  (declare (ignore ctx))
  (dolist (a args) (ensure-integer "mod" a))
  (when (zerop (second args))
    (error 'wardlisp-type-error :message "mod: division by zero"))
  (rem (first args) (second args)))

(defun builtin-fdiv (args ctx)
  "Built-in / function. Returns integer when both args are integers and
divisible, otherwise returns float."
  (dolist (a args) (ensure-number "/" a))
  (when (zerop (second args))
    (error 'wardlisp-type-error :message "/: division by zero"))
  (let ((a (first args)) (b (second args)))
    (if (and (integerp a) (integerp b) (zerop (rem a b)))
        (check-integer ctx (cl:/ a b))
        (check-integer ctx
                       (call-checking-float-range
                        "/" (lambda ()
                              (coerce (cl:/ (coerce a 'double-float)
                                            (coerce b 'double-float))
                                      'double-float)))))))

;;; --- Comparison ---

(defun builtin-num-eq (args ctx)
  "Built-in = function. Numeric equality.
Returns nil if either argument is not a number."
  (declare (ignore ctx))
  (let ((a (first args)) (b (second args)))
    (if (and (numberp a) (numberp b) (cl:= a b)) t nil)))

(defun builtin-lt (args ctx)
  "Built-in < function. Numeric less-than.
Returns nil if either argument is not a number."
  (declare (ignore ctx))
  (let ((a (first args)) (b (second args)))
    (if (and (numberp a) (numberp b) (cl:< a b)) t nil)))

(defun builtin-le (args ctx)
  "Built-in <= function. Numeric less-than-or-equal.
Returns nil if either argument is not a number."
  (declare (ignore ctx))
  (let ((a (first args)) (b (second args)))
    (if (and (numberp a) (numberp b) (cl:<= a b)) t nil)))

(defun builtin-gt (args ctx)
  "Built-in > function. Numeric greater-than.
Returns nil if either argument is not a number."
  (declare (ignore ctx))
  (let ((a (first args)) (b (second args)))
    (if (and (numberp a) (numberp b) (cl:> a b)) t nil)))

(defun builtin-ge (args ctx)
  "Built-in >= function. Numeric greater-than-or-equal.
Returns nil if either argument is not a number."
  (declare (ignore ctx))
  (let ((a (first args)) (b (second args)))
    (if (and (numberp a) (numberp b) (cl:>= a b)) t nil)))

(defun builtin-integer-p (args ctx)
  "Built-in integer? predicate. Returns t if argument is an integer."
  (declare (ignore ctx))
  (if (integerp (first args)) t nil))

(defun builtin-number-p (args ctx)
  "Built-in number? predicate. Returns t if argument is a number (integer or float)."
  (declare (ignore ctx))
  (if (numberp (first args)) t nil))

;;; --- List operations ---

(defun builtin-cons (args ctx)
  "Built-in cons function. Create an ocons pair."
  (track-cons ctx)
  (make-ocons (first args) (second args)))

(defun builtin-car (args ctx)
  "Built-in car function. Return first element of pair."
  (declare (ignore ctx))
  (let ((pair (first args)))
    (if (null pair) nil
        (progn (ensure-ocons "car" pair) (ocons-ocar pair)))))

(defun builtin-cdr (args ctx)
  "Built-in cdr function. Return rest of pair."
  (declare (ignore ctx))
  (let ((pair (first args)))
    (if (null pair) nil
        (progn (ensure-ocons "cdr" pair) (ocons-ocdr pair)))))

(defun builtin-list (args ctx)
  "Built-in list function. Create an ocons list from arguments."
  (let ((result nil))
    (dolist (a (reverse args) result)
      (track-cons ctx)
      (setf result (make-ocons a result)))))

(defun builtin-null-p (args ctx)
  "Built-in null? predicate."
  (declare (ignore ctx))
  (if (null (first args)) t nil))

(defun builtin-atom-p (args ctx)
  "Built-in atom? predicate."
  (declare (ignore ctx))
  (if (ocons-p (first args)) nil t))

(defun builtin-length (args ctx)
  "Built-in length function. Count elements in an ocons list, subject to
max-integer like any other number."
  (let ((lst (first args)) (count 0))
    (unless (or (null lst) (ocons-p lst))
      (error 'wardlisp-type-error :message
             (format nil "length: expected list, got ~a" (describe-value lst))))
    (loop while (ocons-p lst)
          do (incf count) (setf lst (ocons-ocdr lst)))
    (when lst
      (error 'wardlisp-type-error :message
             (format nil "length: improper list with tail ~a" (describe-value lst))))
    (check-integer ctx count)))

(defun builtin-append (args ctx)
  "Built-in append function. Concatenate zero or more lists."
  (if (null args)
      nil
      (let ((result (car (last args))))
        (loop for lst in (nreverse (butlast args))
              do (setf result (ocons-prepend lst result ctx)))
        result)))

(defun ocons-prepend (lst tail ctx)
  "Copy ocons list LST and attach TAIL at the end. Iterative."
  (if (null lst)
      tail
      (progn
       (unless (ocons-p lst)
         (error 'wardlisp-type-error :message
                (format nil "append: expected list, got ~a"
                        (describe-value lst))))
       (let* ((head (progn (track-cons ctx) (make-ocons (ocons-ocar lst) nil)))
              (current head))
         (setf lst (ocons-ocdr lst))
         (loop while (ocons-p lst)
               do (let ((new
                         (progn
                          (track-cons ctx)
                          (make-ocons (ocons-ocar lst) nil))))
                    (setf (ocons-ocdr current) new)
                    (setf current new)
                    (setf lst (ocons-ocdr lst))))
         (when (and lst (not (null lst)))
           (error 'wardlisp-type-error :message
                  (format nil "append: improper list with tail ~a"
                          (describe-value lst))))
         (setf (ocons-ocdr current) tail)
         head))))

;;; --- Other ---

(defun builtin-not (args ctx)
  "Built-in not function. Boolean negation."
  (declare (ignore ctx))
  (if (first args) nil t))

(defun builtin-eq-p (args ctx)
  "Built-in eq? function. Structural equality."
  (declare (ignore ctx))
  (let ((a (first args)) (b (second args)))
    (if (cond ((and (numberp a) (numberp b)) (cl:= a b))
              ((and (stringp a) (stringp b)) (string= a b))
              ((and (wstring-p a) (wstring-p b))
               (string= (wstring-value a) (wstring-value b)))
              (t (eql a b)))
        t
        nil)))

(defun builtin-equal-p (args ctx)
  "Built-in equal? function. Deep structural equality.  Each pair of cells
compared costs one step, so comparing structure that is shared many times over
is bounded by fuel."
  (if (wardlisp-equal (first args) (second args) 0 ctx) t nil))

(defun wardlisp-equal (a b depth ctx)
  "Structural comparison: iterates cdr-spine, recurses car-branches.  Consumes
one step of CTX's fuel per pair of cells compared."
  (loop
    (when (> depth 10000)
      (error 'wardlisp-recursion-limit-exceeded :message
             "equal?: comparison too deep"))
    (cond
      ((eql a b) (return t))
      ((and (numberp a) (numberp b)) (return (= a b)))
      ((and (stringp a) (stringp b)) (return (string= a b)))
      ((and (wstring-p a) (wstring-p b))
       (return (string= (wstring-value a) (wstring-value b))))
      ((and (ocons-p a) (ocons-p b))
       (consume-fuel ctx)
       (unless (wardlisp-equal (ocons-ocar a) (ocons-ocar b) (1+ depth) ctx)
         (return nil))
       ;; Following the cdr is iteration, not nesting: DEPTH is unchanged.
       (setf a (ocons-ocdr a))
       (setf b (ocons-ocdr b)))
      (t (return nil)))))

(defun builtin-print (args ctx)
  "Built-in print function. Append string representation to output buffer.
The value is printed only as far as the output budget allows, so a value whose
printed form is enormous stops at the limit instead of being built whole."
  (let* ((value (first args))
         (output (exec-ctx-output ctx))
         (budget (- (exec-ctx-max-output ctx) (length output) 1)))
    (multiple-value-bind (str complete)
        (if (minusp budget) (values "" nil) (print-value-limited value budget))
      (unless complete
        (error 'wardlisp-output-limit-exceeded
               :message "Output limit exceeded"))
      (loop for ch across str do (vector-push-extend ch output))
      (vector-push-extend #\Newline output))
    value))

(defun builtin-random (args ctx)
  "Built-in random function. Returns a non-negative integer in [0, N) for
positive integer N. Uses the dynamic *random-state*; bind via the
EVALUATE :random-seed keyword for reproducible sequences."
  (declare (ignore ctx))
  (let ((n (first args)))
    (ensure-integer "random" n)
    (unless (plusp n)
      (error 'wardlisp-type-error
             :message (format nil "random: expected positive integer, got ~a" (describe-value n))))
    (random n)))

;;; --- String operations ---

(defun builtin-string-append (args ctx)
  "Built-in string-append. Concatenate string values into a new string.
Charges memory proportional to the result length."
  (dolist (a args) (ensure-wstring "string-append" a))
  (let ((total (reduce #'+ args :key (lambda (s) (length (wstring-value s)))
                       :initial-value 0)))
    (track-cons ctx (max 1 total))
    (let ((out (make-string-output-stream)))
      (dolist (a args) (write-string (wstring-value a) out))
      (make-wstring (get-output-stream-string out)))))

(defun builtin-number->string (args ctx)
  "Built-in number->string. Convert a number to its decimal string.
Charges memory proportional to the result length."
  (let ((n (first args)))
    (ensure-number "number->string" n)
    (let ((s (print-value n)))
      (track-cons ctx (max 1 (length s)))
      (make-wstring s))))

(defun builtin-string-length (args ctx)
  "Built-in string-length. Return the number of characters in a string, subject
to max-integer like any other number."
  (let ((s (first args)))
    (ensure-wstring "string-length" s)
    (check-integer ctx (length (wstring-value s)))))

;;; --- Value printing ---

(defun format-float (value)
  "Format a float for wardlisp output. Removes trailing zeros but keeps at least one decimal."
  (let ((s (format nil "~f" value)))
    (let ((dot-pos (position #\. s)))
      (if dot-pos
          (let ((end (length s)))
            (loop while (and (> end (+ dot-pos 2))
                             (char= (char s (1- end)) #\0))
                  do (decf end))
            (subseq s 0 end))
          s))))

(defvar *print-remaining* nil
  "Characters WRITE-VALUE may still produce, or NIL for no limit.")

(define-condition print-limit-reached (error) ()
  (:documentation "Signalled by EMIT when *PRINT-REMAINING* runs out."))

(defun emit (text stream)
  "Write TEXT to STREAM, charging it to *PRINT-REMAINING*.  When TEXT would
overdraw the budget, only the part that fits is written and PRINT-LIMIT-REACHED
is signalled, so a long atom is cut rather than dropped."
  (when *print-remaining*
    (when (> (length text) *print-remaining*)
      (write-string text stream :end *print-remaining*)
      (setf *print-remaining* 0)
      (error 'print-limit-reached))
    (decf *print-remaining* (length text)))
  (write-string text stream))

(defun write-value (value stream depth)
  "Write VALUE's printed form to STREAM through EMIT.  Every node writes at least
one character, so the work done is bounded by the characters written."
  (if (> depth 100)
      (emit "..." stream)
      (cond ((null value) (emit "nil" stream))
            ((eq value t) (emit "t" stream))
            ((integerp value) (emit (format nil "~d" value) stream))
            ((floatp value) (emit (format-float value) stream))
            ((wstring-p value) (emit (print-wstring value) stream))
            ((stringp value) (emit value stream))
            ((ocons-p value) (write-ocons value stream (1+ depth)))
            ((closure-p value)
             (emit (format nil "#<closure~@[ ~a~]>" (closure-name value)) stream))
            ((builtin-p value)
             (emit (format nil "#<builtin ~a>" (builtin-name value)) stream))
            (t (emit "#<unknown>" stream)))))

(defun print-value-limited (value limit)
  "Return the printed form of VALUE cut to at most LIMIT characters, and as a
second value whether it is complete.  The work done is bounded by LIMIT, however
much structure VALUE shares."
  (let ((stream (make-string-output-stream))
        (*print-remaining* limit))
    (handler-case (progn (write-value value stream 0)
                         (values (get-output-stream-string stream) t))
      (print-limit-reached ()
        (values (get-output-stream-string stream) nil)))))

(defun describe-with (writer object)
  "Return OBJECT as WRITER writes it, for an error message: at most 200
characters, with \"...\" when cut."
  (let ((stream (make-string-output-stream))
        (*print-remaining* 200))
    (handler-case (progn (funcall writer object stream 0)
                         (get-output-stream-string stream))
      (print-limit-reached ()
        (concatenate 'string (get-output-stream-string stream) "...")))))

(defun describe-value (value)
  "Return the runtime VALUE's printed form for an error message: at most 200
characters, with \"...\" when cut."
  (describe-with #'write-value value))

(defun write-form (form stream depth)
  "Write the source FORM -- a reader AST, whose lists are CL lists -- to STREAM in
wardlisp syntax through EMIT."
  (cond ((> depth 100) (emit "..." stream))
        ((consp form)
         (emit "(" stream)
         (write-form (car form) stream (1+ depth))
         (let ((rest (cdr form)))
           (loop while (consp rest)
                 do (emit " " stream)
                    (write-form (car rest) stream (1+ depth))
                    (setf rest (cdr rest)))
           (when rest
             (emit " . " stream)
             (write-form rest stream (1+ depth))))
         (emit ")" stream))
        (t (write-value form stream depth))))

(defun describe-form (form)
  "Return the source FORM in wardlisp syntax for an error message: at most 200
characters, with \"...\" when cut."
  (describe-with #'write-form form))

(defun print-value (value &optional (depth 0))
  "Convert a runtime value to its string representation.
Limits nesting depth to prevent host stack overflow.  The text is not bounded
otherwise: a value that shares structure, such as (list y y) nested N deep, has
a printed form exponential in N.  Inside evaluation use PRINT-VALUE-LIMITED."
  (let ((*print-remaining* nil))
    (with-output-to-string (stream)
      (write-value value stream depth))))

(defun write-ocons (cell stream depth)
  "Write an ocons chain to STREAM as a list or dotted pair."
  (if (> depth 100)
      (emit "(...)" stream)
      (progn
        (emit "(" stream)
        (write-value (ocons-ocar cell) stream depth)
        (let ((rest (ocons-ocdr cell)))
          (loop while (ocons-p rest)
                do (emit " " stream)
                   (write-value (ocons-ocar rest) stream depth)
                   (setf rest (ocons-ocdr rest)))
          (unless (null rest)
            (emit " . " stream)
            (write-value rest stream depth)))
        (emit ")" stream))))

(defun print-wstring (ws)
  "Print a boxed string as a double-quoted literal, re-escaping \" \\ newline tab."
  (with-output-to-string (s)
    (write-char #\" s)
    (loop for ch across (wstring-value ws)
          do (case ch
               (#\" (write-string "\\\"" s))
               (#\\ (write-string "\\\\" s))
               (#\Newline (write-string "\\n" s))
               (#\Tab (write-string "\\t" s))
               (t (write-char ch s))))
    (write-char #\" s)))
