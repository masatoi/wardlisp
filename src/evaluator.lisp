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
                               (max-integer (expt 2 64))
                               (max-expr-depth 1000))
  "Parse and evaluate INPUT string. Returns the result of the last expression."
  (let ((program (wardlisp-read-program input))
        (ctx (make-exec-ctx :fuel fuel :max-depth max-depth
                            :max-cons max-cons :max-output max-output
                            :max-integer max-integer
                            :max-expr-depth max-expr-depth))
        (env (make-initial-env)))
    (let ((result nil))
      (dolist (expr program result)
        (setf result (wardlisp-eval expr env ctx))))))

(defun wardlisp-eval (expr env ctx)
  "Evaluate EXPR in ENV with execution context CTX.
Resolves all tail-calls via trampoline. Always returns a final value."
  (let ((result (eval-inner expr env ctx)))
    (if (tail-call-p result)
        (trampoline result ctx)
        result)))

(defun trampoline (tc ctx)
  "Resolve a chain of tail-calls in constant stack space."
  (track-depth ctx 1)
  (unwind-protect
       (loop
         (consume-fuel ctx)
         (let ((result (eval-inner (tail-call-expr tc) (tail-call-env tc) ctx)))
           (if (tail-call-p result)
               (setf tc result)
               (return result))))
    (track-depth ctx -1)))

(defun eval-inner (expr env ctx)
  "Single-step evaluation. May return tail-call structs for closure applications.
Use wardlisp-eval when you need a fully resolved value."
  (consume-fuel ctx)
  (cond
    ((integerp expr) (check-integer ctx expr))
    ((eq expr t) t)
    ((null expr) nil)
    ((stringp expr) (env-lookup env expr))
    ((consp expr)
     (track-expr-depth ctx 1)
     (unwind-protect
          (eval-compound expr env ctx)
       (track-expr-depth ctx -1)))
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
          ((string= head "apply") (eval-apply args env ctx))
          (t (eval-application head args env ctx)))
        (eval-application head args env ctx))))

;;; --- Special forms ---

(defun eval-quote (args ctx)
  "Evaluate a quote form."
  (when (/= (length args) 1)
    (error 'wardlisp-arity-error :message "quote requires exactly 1 argument"))
  (ast-to-value (car args) ctx))

(defun ast-to-value (ast ctx &optional (depth 0))
  "Convert a quoted AST to a runtime value (ocons chain for lists)."
  (when (> depth +max-parse-depth+)
    (error 'wardlisp-parse-error
           :message "Quoted expression nesting too deep"))
  (cond ((null ast) nil) ((integerp ast) (check-integer ctx ast))
        ((eq ast t) t) ((stringp ast) ast)
        ((consp ast) (track-cons ctx)
         (make-ocons (ast-to-value (car ast) ctx (1+ depth))
                     (ast-to-value (cdr ast) ctx (1+ depth))))
        (t ast)))

(defun eval-if (args env ctx)
  "Evaluate an if form with 2 or 3 arguments.
Then/else branches are in tail position (use eval-inner)."
  (let ((len (length args)))
    (when (or (< len 2) (> len 3))
      (error 'wardlisp-arity-error :message "if requires 2 or 3 arguments"))
    (if (wardlisp-eval (first args) env ctx)
        (eval-inner (second args) env ctx)
        (if (= len 3)
            (eval-inner (third args) env ctx)
            nil))))

(defun eval-let (args env ctx)
  "Evaluate a let form with parallel bindings.
All right-hand sides are evaluated in the outer environment before binding.
Last body expression is in tail position (use eval-inner)."
  (when (< (length args) 2)
    (error 'wardlisp-arity-error :message "let requires bindings and body"))
  (let ((bindings (first args)) (body (rest args)))
    (unless (listp bindings)
      (error 'wardlisp-parse-error :message
             (format nil "let: bindings must be a list, got ~s" bindings)))
    (dolist (binding bindings)
      (unless (and (listp binding) (= 2 (length binding)) (stringp (first binding)))
        (error 'wardlisp-parse-error :message
               (format nil "let: each binding must be (name expr), got ~s" binding))))
    ;; Evaluate all RHS in outer environment first (parallel semantics)
    (let* ((names (mapcar #'first bindings))
           (vals (mapcar (lambda (b) (wardlisp-eval (second b) env ctx)) bindings))
           (new-env (env-extend env names vals)))
      (let ((result nil))
        (loop for (expr . rest) on body
              do (setf result
                       (if rest
                           (wardlisp-eval expr new-env ctx)
                           (eval-inner expr new-env ctx))))
        result))))

(defun eval-let* (args env ctx)
  "Evaluate a let* form with sequential bindings.
Each binding is visible to subsequent ones.
Last body expression is in tail position (use eval-inner)."
  (when (< (length args) 2)
    (error 'wardlisp-arity-error :message "let* requires bindings and body"))
  (let ((bindings (first args)) (body (rest args)) (current-env env))
    (unless (listp bindings)
      (error 'wardlisp-parse-error :message
             (format nil "let*: bindings must be a list, got ~s" bindings)))
    (dolist (binding bindings)
      (unless (and (listp binding) (= 2 (length binding)) (stringp (first binding)))
        (error 'wardlisp-parse-error :message
               (format nil "let*: each binding must be (name expr), got ~s" binding)))
      (let ((val (wardlisp-eval (second binding) current-env ctx)))
        (setf current-env
                (env-extend current-env (list (first binding)) (list val)))))
    (let ((result nil))
      (loop for (expr . rest) on body
            do (setf result
                       (if rest
                           (wardlisp-eval expr current-env ctx)
                           (eval-inner expr current-env ctx))))
      result)))

(defun eval-lambda (args env)
  "Evaluate a lambda form, creating a closure."
  (when (< (length args) 2)
    (error 'wardlisp-arity-error :message "lambda requires params and body"))
  (let ((params (first args)))
    (unless (listp params)
      (error 'wardlisp-parse-error :message
             (format nil "lambda: params must be a list, got ~s" params)))
    (dolist (p params)
      (unless (stringp p)
        (error 'wardlisp-parse-error :message
               (format nil "lambda: param must be a symbol, got ~s" p))))
    (let ((body
           (if (= 1 (length (rest args)))
               (second args)
               (cons "begin" (rest args)))))
      (make-closure params body env))))

(defun eval-define (args env ctx)
  "Handle (define name value) and (define (name params...) body...).
Redefinition updates existing user binding rather than appending.
Builtin bindings (first frame) are not overwritable."
  (labels ((update-or-append (name value)
             ;; Search user frames (skip first frame = builtins)
             (loop for frame in (rest env)
                   do (let ((pair (assoc name frame :test #'string=)))
                        (when pair
                          (setf (cdr pair) value)
                          (return-from update-or-append))))
             ;; Not found in user frames, append new frame
             (nconc env (list (list (cons name value))))))
    (let ((target (first args)))
      (cond
       ((consp target)
        (let ((name (first target))
              (params (rest target)))
          (unless (stringp name)
            (error 'wardlisp-parse-error :message
                   (format nil "define: function name must be a symbol, got ~s" name)))
          (dolist (p params)
            (unless (stringp p)
              (error 'wardlisp-parse-error :message
                     (format nil "define: parameter must be a symbol, got ~s" p))))
          (when (< (length (rest args)) 1)
            (error 'wardlisp-arity-error :message
                   "define: function form requires a body"))
          (let* ((body
                  (if (= 1 (length (rest args)))
                      (second args)
                      (cons "begin" (rest args))))
                 (closure (make-closure params body env name)))
            (let ((new-env (env-extend env (list name) (list closure))))
              (setf (closure-env closure) new-env)
              (update-or-append name closure)
              closure))))
       ((stringp target)
        (unless (= (length args) 2)
          (error 'wardlisp-arity-error :message
                 (format nil "define: expected (define name value), got ~d argument~:p"
                         (length args))))
        (let ((value (wardlisp-eval (second args) env ctx)))
          (update-or-append target value)
          value))
       (t
        (error 'wardlisp-parse-error :message
               (format nil "Invalid define target: ~s" target)))))))

(defun eval-begin (args env ctx)
  "Evaluate a begin form.
Last expression is in tail position (use eval-inner)."
  (let ((result nil))
    (loop for (expr . rest) on args
          do (setf result (if rest
                              (wardlisp-eval expr env ctx)
                              (eval-inner expr env ctx))))
    result))

(defun eval-cond (clauses env ctx)
  "Evaluate a cond form.
Result expressions are in tail position (use eval-inner)."
  (dolist (clause clauses nil)
    (unless (listp clause)
      (error 'wardlisp-parse-error :message
             (format nil "cond: each clause must be a list, got ~s" clause)))
    (let ((test-result (wardlisp-eval (first clause) env ctx)))
      (when test-result
        (return
         (if (rest clause)
             (let ((result nil))
               (loop for (expr . remaining) on (rest clause)
                     do (setf result
                              (if remaining
                                  (wardlisp-eval expr env ctx)
                                  (eval-inner expr env ctx))))
               result)
             test-result))))))

(defun eval-and (args env ctx)
  "Evaluate an and form with short-circuit semantics.
Last argument is in tail position (use eval-inner)."
  (if (null args) t
      (let ((result nil))
        (loop for (arg . rest) on args
              do (setf result (if rest
                                  (wardlisp-eval arg env ctx)
                                  (eval-inner arg env ctx)))
              unless result return nil)
        result)))

(defun eval-or (args env ctx)
  "Evaluate an or form with short-circuit semantics.
Last argument is in tail position (use eval-inner)."
  (if (null args) nil
      (loop for (arg . rest) on args
            for result = (if rest
                             (wardlisp-eval arg env ctx)
                             (eval-inner arg env ctx))
            when result return result)))

;;; --- Function application ---

(defun eval-apply (args env ctx)
  "Evaluate (apply func arg-list). Converts ocons list to args and dispatches."
  (when (/= (length args) 2)
    (error 'wardlisp-arity-error :message "apply requires exactly 2 arguments"))
  (let ((func (wardlisp-eval (first args) env ctx))
        (arg-list (wardlisp-eval (second args) env ctx)))
    (let ((cl-args nil))
      (loop while (ocons-p arg-list)
            do (push (ocons-ocar arg-list) cl-args)
               (setf arg-list (ocons-ocdr arg-list)))
      (when arg-list
        (error 'wardlisp-type-error
               :message "apply: second argument must be a proper list"))
      (apply-function func (nreverse cl-args) ctx))))

(defun eval-application (operator args env ctx)
  "Evaluate a function call."
  (let ((func (wardlisp-eval operator env ctx))
        (evaluated-args (mapcar (lambda (a) (wardlisp-eval a env ctx)) args)))
    (apply-function func evaluated-args ctx)))

(defun apply-function (func args ctx)
  "Apply FUNC to ARGS. Returns tail-call struct for closures (trampolined)."
  (consume-fuel ctx 4)
  (cond
    ((closure-p func)
     (let ((params (closure-params func)))
       (when (/= (length params) (length args))
         (error 'wardlisp-arity-error
                :message (format nil "~a expects ~d arg~:p, got ~d"
                                 (or (closure-name func) "lambda")
                                 (length params) (length args))))
       (let ((call-env (env-extend (closure-env func) params args)))
         (make-tail-call :expr (closure-body func) :env call-env))))
    ((builtin-p func)
     (when (and (builtin-arity func)
                (/= (builtin-arity func) (length args)))
       (error 'wardlisp-arity-error
              :message (format nil "~a expects ~d arg~:p, got ~d"
                               (builtin-name func)
                               (builtin-arity func) (length args))))
     (funcall (builtin-func func) args ctx))
    (t (error 'wardlisp-type-error
              :message (format nil "Not a function: ~a" (print-value func))))))
