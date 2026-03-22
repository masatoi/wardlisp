(defpackage :wardlisp/src/evaluator
  (:use :cl
        :wardlisp/src/types
        :wardlisp/src/reader
        :wardlisp/src/env
        :wardlisp/src/builtins)
  (:export #:wardlisp-eval #:eval-string))
(in-package :wardlisp/src/evaluator)

(defun eval-string (input &key (fuel 10000) (max-depth 100)
                               (max-cons 10000) (max-output 1000)
                               (max-integer (expt 2 64)))
  "Parse and evaluate INPUT string. Returns the result of the last expression."
  (let ((program (wardlisp-read-program input))
        (ctx (make-exec-ctx :fuel fuel :max-depth max-depth
                            :max-cons max-cons :max-output max-output
                            :max-integer max-integer))
        (env (make-initial-env)))
    (let ((result nil))
      (dolist (expr program result)
        (setf result (wardlisp-eval expr env ctx))))))

(defun wardlisp-eval (expr env ctx)
  "Evaluate EXPR in ENV with execution context CTX."
  (consume-fuel ctx)
  (cond
    ((integerp expr) (check-integer ctx expr))
    ((eq expr t) t)
    ((null expr) nil)
    ((stringp expr) (env-lookup env expr))
    ((consp expr) (eval-compound expr env ctx))
    (t (error 'wardlisp-internal-error
              :message (format nil "Unknown expression type: ~s" expr)))))

(defun eval-compound (expr env ctx)
  "Evaluate a compound (list) expression."
  (let ((head (car expr))
        (args (cdr expr)))
    ;; head must be a string for special forms
    (if (stringp head)
        (cond
          ((string= head "quote")  (eval-quote args ctx))
          ((string= head "if")     (eval-if args env ctx))
          ((string= head "let")    (eval-let args env ctx))
          ((string= head "let*")   (eval-let* args env ctx))
          ((string= head "lambda") (eval-lambda args env))
          ((string= head "define") (eval-define args env ctx))
          ((string= head "cond")   (eval-cond args env ctx))
          ((string= head "begin")  (eval-begin args env ctx))
          ((string= head "and")    (eval-and args env ctx))
          ((string= head "or")     (eval-or args env ctx))
          (t (eval-application head args env ctx)))
        (eval-application head args env ctx))))

;;; --- Special forms ---

(defun eval-quote (args ctx)
  "Evaluate a quote form."
  (when (/= (length args) 1)
    (error 'wardlisp-arity-error :message "quote requires exactly 1 argument"))
  (ast-to-value (car args) ctx))

(defun ast-to-value (ast ctx)
  "Convert a quoted AST to a runtime value (ocons chain for lists)."
  (cond
    ((null ast) nil)
    ((integerp ast) (check-integer ctx ast))
    ((eq ast t) t)
    ((stringp ast) ast)
    ((consp ast)
     (track-cons ctx)
     (make-ocons (ast-to-value (car ast) ctx)
                 (ast-to-value (cdr ast) ctx)))
    (t ast)))

(defun eval-if (args env ctx)
  "Evaluate an if form with 2 or 3 arguments."
  (let ((len (length args)))
    (when (or (< len 2) (> len 3))
      (error 'wardlisp-arity-error :message "if requires 2 or 3 arguments"))
    (if (wardlisp-eval (first args) env ctx)
        (wardlisp-eval (second args) env ctx)
        (if (= len 3)
            (wardlisp-eval (third args) env ctx)
            nil))))

(defun eval-let (args env ctx)
  "Evaluate a let form with sequential bindings (Clojure-style)."
  (when (< (length args) 2)
    (error 'wardlisp-arity-error :message "let requires bindings and body"))
  (let ((bindings (first args))
        (body (rest args))
        (current-env env))
    (dolist (binding bindings)
      (let ((val (wardlisp-eval (second binding) current-env ctx)))
        (setf current-env (env-extend current-env
                                      (list (first binding))
                                      (list val)))))
    (let ((result nil))
      (dolist (expr body result)
        (setf result (wardlisp-eval expr current-env ctx))))))

(defun eval-let* (args env ctx)
  "Evaluate a let* form (alias for let)."
  (eval-let args env ctx))

(defun eval-lambda (args env)
  "Evaluate a lambda form, creating a closure."
  (when (< (length args) 2)
    (error 'wardlisp-arity-error :message "lambda requires params and body"))
  (let ((params (first args))
        (body (if (= 1 (length (rest args)))
                  (second args)
                  (cons "begin" (rest args)))))
    (make-closure params body env)))

(defun eval-define (args env ctx)
  "Handle (define name value) and (define (name params...) body...)."
  (let ((target (first args)))
    (cond
      ;; (define (f x y) body...) — function definition
      ((consp target)
       (let* ((name (first target))
              (params (rest target))
              (body (if (= 1 (length (rest args)))
                        (second args)
                        (cons "begin" (rest args))))
              (closure (make-closure params body env name)))
         ;; Self-reference: put closure in its own env
         (let ((new-env (env-extend env (list name) (list closure))))
           (setf (closure-env closure) new-env)
           ;; Also add to calling env for subsequent defines
           (let ((frame (list (cons name closure))))
             (nconc env (list frame)))
           closure)))
      ;; (define name value)
      ((stringp target)
       (let ((value (wardlisp-eval (second args) env ctx)))
         (let ((frame (list (cons target value))))
           (nconc env (list frame)))
         value))
      (t (error 'wardlisp-parse-error
                :message (format nil "Invalid define target: ~s" target))))))

(defun eval-begin (args env ctx)
  "Evaluate a begin form. Returns the value of the last expression."
  (let ((result nil))
    (dolist (expr args result)
      (setf result (wardlisp-eval expr env ctx)))))

(defun eval-cond (clauses env ctx)
  "Evaluate a cond form."
  (dolist (clause clauses nil)
    (let ((test-result (wardlisp-eval (first clause) env ctx)))
      (when test-result
        (return (if (rest clause)
                    (wardlisp-eval (second clause) env ctx)
                    test-result))))))

(defun eval-and (args env ctx)
  "Evaluate an and form with short-circuit semantics."
  (if (null args) t
      (let ((result nil))
        (dolist (arg args result)
          (setf result (wardlisp-eval arg env ctx))
          (unless result (return nil))))))

(defun eval-or (args env ctx)
  "Evaluate an or form with short-circuit semantics."
  (if (null args) nil
      (dolist (arg args nil)
        (let ((result (wardlisp-eval arg env ctx)))
          (when result (return result))))))

;;; --- Function application ---

(defun eval-application (operator args env ctx)
  "Evaluate a function call."
  (let ((func (wardlisp-eval operator env ctx))
        (evaluated-args (mapcar (lambda (a) (wardlisp-eval a env ctx)) args)))
    (apply-function func evaluated-args ctx)))

(defun apply-function (func args ctx)
  "Apply FUNC to ARGS."
  (consume-fuel ctx 4)
  (cond
    ((closure-p func)
     (let ((params (closure-params func)))
       (when (/= (length params) (length args))
         (error 'wardlisp-arity-error
                :message (format nil "~a expects ~d args, got ~d"
                                 (or (closure-name func) "lambda")
                                 (length params) (length args))))
       (track-depth ctx 1)
       (unwind-protect
            (let ((call-env (env-extend (closure-env func) params args)))
              (wardlisp-eval (closure-body func) call-env ctx))
         (track-depth ctx -1))))
    ((builtin-p func)
     (when (and (builtin-arity func)
                (/= (builtin-arity func) (length args)))
       (error 'wardlisp-arity-error
              :message (format nil "~a expects ~d args, got ~d"
                               (builtin-name func)
                               (builtin-arity func) (length args))))
     (funcall (builtin-func func) args ctx))
    (t (error 'wardlisp-type-error
              :message (format nil "Not a function: ~s" func)))))
