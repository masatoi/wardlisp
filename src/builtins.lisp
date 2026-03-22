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
           (cons "div" (make-builtin "div" #'builtin-div 2))
           (cons "mod" (make-builtin "mod" #'builtin-mod 2))
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
           (cons "print" (make-builtin "print" #'builtin-print 1)))))
    (list builtins)))

;;; --- Helpers ---

(defun ensure-integer (name value)
  "Ensure VALUE is an integer, or signal a type error."
  (unless (integerp value)
    (error 'wardlisp-type-error
           :message (format nil "~a: expected integer, got ~a" name (print-value value)))))

(defun ensure-ocons (name value)
  "Ensure VALUE is an ocons pair, or signal a type error."
  (unless (ocons-p value)
    (error 'wardlisp-type-error
           :message (format nil "~a: expected pair, got ~a" name (print-value value)))))

;;; --- Arithmetic ---

(defun builtin-add (args ctx)
  "Built-in + function. Variadic addition."
  (dolist (a args) (ensure-integer "+" a))
  (check-integer ctx (reduce #'+ args :initial-value 0)))

(defun builtin-sub (args ctx)
  "Built-in - function. Unary negation or variadic subtraction."
  (when (null args)
    (error 'wardlisp-arity-error :message "- requires at least 1 argument"))
  (dolist (a args) (ensure-integer "-" a))
  (check-integer ctx (if (= 1 (length args))
                         (- (first args))
                         (reduce #'- args))))

(defun builtin-mul (args ctx)
  "Built-in * function. Variadic multiplication."
  (dolist (a args) (ensure-integer "*" a))
  (check-integer ctx (reduce #'* args :initial-value 1)))

(defun builtin-div (args ctx)
  "Built-in div function. Integer division (truncate)."
  (dolist (a args) (ensure-integer "div" a))
  (when (zerop (second args))
    (error 'wardlisp-type-error :message "div: division by zero"))
  (check-integer ctx (truncate (first args) (second args))))

(defun builtin-mod (args ctx)
  "Built-in mod function. Integer modulo."
  (declare (ignore ctx))
  (dolist (a args) (ensure-integer "mod" a))
  (when (zerop (second args))
    (error 'wardlisp-type-error :message "mod: division by zero"))
  (cl:mod (first args) (second args)))

;;; --- Comparison ---

(defun builtin-num-eq (args ctx)
  "Built-in = function. Numeric equality."
  (declare (ignore ctx))
  (dolist (a args) (ensure-integer "=" a))
  (if (cl:= (first args) (second args)) t nil))

(defun builtin-lt (args ctx)
  "Built-in < function. Numeric less-than."
  (declare (ignore ctx))
  (dolist (a args) (ensure-integer "<" a))
  (if (cl:< (first args) (second args)) t nil))

(defun builtin-le (args ctx)
  "Built-in <= function. Numeric less-than-or-equal."
  (declare (ignore ctx))
  (dolist (a args) (ensure-integer "<=" a))
  (if (cl:<= (first args) (second args)) t nil))

(defun builtin-gt (args ctx)
  "Built-in > function. Numeric greater-than."
  (declare (ignore ctx))
  (dolist (a args) (ensure-integer ">" a))
  (if (cl:> (first args) (second args)) t nil))

(defun builtin-ge (args ctx)
  "Built-in >= function. Numeric greater-than-or-equal."
  (declare (ignore ctx))
  (dolist (a args) (ensure-integer ">=" a))
  (if (cl:>= (first args) (second args)) t nil))

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
    (loop while (ocons-p lst)
          do (incf count) (setf lst (ocons-ocdr lst)))
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
          (error 'wardlisp-type-error
                 :message (format nil "append: expected list, got ~a" (print-value lst))))
        (let* ((head (progn (track-cons ctx) (make-ocons (ocons-ocar lst) nil)))
               (current head))
          (setf lst (ocons-ocdr lst))
          (loop while (ocons-p lst)
                do (let ((new (progn (track-cons ctx) (make-ocons (ocons-ocar lst) nil))))
                     (setf (ocons-ocdr current) new)
                     (setf current new)
                     (setf lst (ocons-ocdr lst))))
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
    (if (cond
          ((and (integerp a) (integerp b)) (cl:= a b))
          ((and (stringp a) (stringp b)) (string= a b))
          (t (eql a b)))
        t nil)))

(defun builtin-equal-p (args ctx)
  "Built-in equal? function. Deep structural equality."
  (declare (ignore ctx))
  (if (wardlisp-equal (first args) (second args) 0) t nil))

(defun wardlisp-equal (a b depth)
  "Recursive structural comparison with depth limit."
  (when (> depth 10000)
    (error 'wardlisp-recursion-limit-exceeded
           :message "equal?: comparison too deep"))
  (cond
    ((and (null a) (null b)) t)
    ((and (eq a t) (eq b t)) t)
    ((and (integerp a) (integerp b)) (cl:= a b))
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

(defun print-value (value)
  "Convert a runtime value to its string representation."
  (cond
    ((null value) "nil")
    ((eq value t) "t")
    ((integerp value) (format nil "~d" value))
    ((stringp value) value)
    ((ocons-p value) (print-ocons value))
    ((closure-p value) (format nil "#<closure~@[ ~a~]>" (closure-name value)))
    ((builtin-p value) (format nil "#<builtin ~a>" (builtin-name value)))
    (t (format nil "#<unknown>"))))

(defun print-ocons (cell)
  "Print an ocons chain as a list or dotted pair."
  (with-output-to-string (s)
    (write-char #\( s)
    (write-string (print-value (ocons-ocar cell)) s)
    (let ((rest (ocons-ocdr cell)))
      (loop while (ocons-p rest)
            do (write-char #\Space s)
               (write-string (print-value (ocons-ocar rest)) s)
               (setf rest (ocons-ocdr rest)))
      (unless (null rest)
        (write-string " . " s)
        (write-string (print-value rest) s)))
    (write-char #\) s)))
