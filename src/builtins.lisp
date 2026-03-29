(defpackage :wardlisp/src/builtins
  (:use :cl :wardlisp/src/types :wardlisp/src/env)
  (:export #:make-initial-env #:print-value))
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
           (cons "print" (make-builtin "print" #'builtin-print 1)))))
    (list builtins)))

;;; --- Helpers ---

(defun ensure-integer (name value)
  "Ensure VALUE is an integer, or signal a type error."
  (unless (integerp value)
    (error 'wardlisp-type-error
           :message (format nil "~a: expected integer, got ~a" name (print-value value)))))

(defun ensure-number (name value)
  "Ensure VALUE is a number (integer or float), or signal a type error."
  (unless (numberp value)
    (error 'wardlisp-type-error
           :message (format nil "~a: expected number, got ~a"
                            name (print-value value)))))

(defun ensure-ocons (name value)
  "Ensure VALUE is an ocons pair, or signal a type error."
  (unless (ocons-p value)
    (error 'wardlisp-type-error
           :message (format nil "~a: expected pair, got ~a" name (print-value value)))))

;;; --- Arithmetic ---

(defun builtin-add (args ctx)
  "Built-in + function. Variadic addition."
  (dolist (a args) (ensure-number "+" a))
  (check-integer ctx (reduce #'+ args :initial-value 0)))

(defun builtin-sub (args ctx)
  "Built-in - function. Unary negation or variadic subtraction."
  (when (null args)
    (error 'wardlisp-arity-error :message "- requires at least 1 argument"))
  (dolist (a args) (ensure-number "-" a))
  (check-integer ctx (if (= 1 (length args))
                         (- (first args))
                         (reduce #'- args))))

(defun builtin-mul (args ctx)
  "Built-in * function. Variadic multiplication."
  (dolist (a args) (ensure-number "*" a))
  (check-integer ctx (reduce #'* args :initial-value 1)))

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
                       (coerce (cl:/ (coerce a 'double-float)
                                     (coerce b 'double-float))
                               'double-float)))))

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
  "Built-in length function. Count elements in an ocons list."
  (declare (ignore ctx))
  (let ((lst (first args)) (count 0))
    (unless (or (null lst) (ocons-p lst))
      (error 'wardlisp-type-error :message
             (format nil "length: expected list, got ~a" (print-value lst))))
    (loop while (ocons-p lst)
          do (incf count) (setf lst (ocons-ocdr lst)))
    (when lst
      (error 'wardlisp-type-error :message
             (format nil "length: improper list with tail ~a" (print-value lst))))
    count))

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
                        (print-value lst))))
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
                          (print-value lst))))
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
              (t (eql a b)))
        t
        nil)))

(defun builtin-equal-p (args ctx)
  "Built-in equal? function. Deep structural equality."
  (declare (ignore ctx))
  (if (wardlisp-equal (first args) (second args) 0) t nil))

(defun wardlisp-equal (a b depth)
  "Recursive structural comparison with depth limit."
  (when (> depth 10000)
    (error 'wardlisp-recursion-limit-exceeded
           :message "equal?: comparison too deep"))
  (cond ((eql a b) t)
        ((and (numberp a) (numberp b)) (cl:= a b))
        ((and (stringp a) (stringp b)) (string= a b))
        ((and (ocons-p a) (ocons-p b))
         (and (wardlisp-equal (ocons-ocar a) (ocons-ocar b) (1+ depth))
              (wardlisp-equal (ocons-ocdr a) (ocons-ocdr b) (1+ depth))))
        (t nil)))

(defun builtin-print (args ctx)
  "Built-in print function. Append string representation to output buffer."
  (let ((value (first args))
        (output (exec-ctx-output ctx)))
    (let ((str (print-value value)))
      (when (> (+ (length output) (length str) 1) (exec-ctx-max-output ctx))
        (error 'wardlisp-output-limit-exceeded
               :message "Output limit exceeded"))
      (loop for ch across str do (vector-push-extend ch output))
      (vector-push-extend #\Newline output))
    value))

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

(defun print-value (value &optional (depth 0))
  "Convert a runtime value to its string representation.
Limits nesting depth to prevent host stack overflow."
  (if (> depth 100)
      "..."
      (cond ((null value) "nil") ((eq value t) "t")
            ((integerp value) (format nil "~d" value))
            ((floatp value) (format-float value))
            ((stringp value) value)
            ((ocons-p value) (print-ocons value (1+ depth)))
            ((closure-p value)
             (format nil "#<closure~@[ ~a~]>" (closure-name value)))
            ((builtin-p value) (format nil "#<builtin ~a>" (builtin-name value)))
            (t (format nil "#<unknown>")))))

(defun print-ocons (cell &optional (depth 0))
  "Print an ocons chain as a list or dotted pair."
  (if (> depth 100)
      "(...)"
      (with-output-to-string (s)
        (write-char #\( s)
        (write-string (print-value (ocons-ocar cell) depth) s)
        (let ((rest (ocons-ocdr cell)))
          (loop while (ocons-p rest)
                do (write-char #\  s)
                   (write-string (print-value (ocons-ocar rest) depth) s)
                   (setf rest (ocons-ocdr rest)))
          (unless (null rest)
            (write-string " . " s)
            (write-string (print-value rest depth) s)))
        (write-char #\) s))))
