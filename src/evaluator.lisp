(defpackage :wardlisp/src/evaluator
  (:use :cl
        :wardlisp/src/types
        :wardlisp/src/reader
        :wardlisp/src/env
        :wardlisp/src/builtins)
  (:export #:wardlisp-eval #:eval-string #:eval-program))
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
    (eval-program program env ctx)))

(defun eval-program (program env ctx)
  "Evaluate top-level PROGRAM expressions in ENV with execution context CTX."
  (let ((result nil))
    (dolist (expr program result)
      (setf result (eval-top-level-form expr env ctx)))))

(defun eval-top-level-form (expr env ctx)
  "Evaluate a single top-level form."
  (if (define-form-p expr)
      (eval-top-level-define (cdr expr) env ctx)
      (wardlisp-eval expr env ctx)))

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
         (let ((result
                 (ecase (tail-call-kind tc)
                   (:expr
                    (eval-inner (tail-call-expr tc) (tail-call-env tc) ctx))
                   (:body
                    (eval-body-forms (tail-call-expr tc) (tail-call-env tc) ctx)))))
           (if (tail-call-p result)
               (setf tc result)
               (return result))))
    (track-depth ctx -1)))

(defun check-not-boolean (context value)
  "Signal a clear error if VALUE is t or nil (reserved, not rebindable)."
  (when (or (eq value t) (eq value nil))
    (error 'wardlisp-parse-error
           :message (format nil "~a: ~a is reserved and cannot be used as a variable name"
                            context (if (eq value t) "t" "nil")))))

(defun define-form-p (expr)
  "Return true when EXPR is a define form."
  (and (consp expr)
       (stringp (car expr))
       (string= (car expr) "define")))

(defun eval-inner (expr env ctx)
  "Single-step evaluation. May return tail-call structs for closure applications.
Use wardlisp-eval when you need a fully resolved value."
  (consume-fuel ctx)
  (cond
    ((numberp expr) (check-integer ctx expr))
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
    (if (stringp head)
        (cond
          ((string= head "quote")  (eval-quote args ctx))
          ((string= head "if")     (eval-if args env ctx))
          ((string= head "let")    (eval-let args env ctx))
          ((string= head "let*")   (eval-let* args env ctx))
          ((string= head "lambda") (eval-lambda args env))
          ((string= head "define")
           (error 'wardlisp-parse-error
                  :message "define is only allowed at top level or at the beginning of a body"))
          ((string= head "cond")   (eval-cond args env ctx))
          ((string= head "begin")  (eval-begin args env ctx))
          ((string= head "and")    (eval-and args env ctx))
          ((string= head "or")     (eval-or args env ctx))
          ((string= head "apply")  (eval-apply args env ctx))
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
  (cond ((null ast) nil) ((numberp ast) (check-integer ctx ast))
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
  "Evaluate a let form with parallel bindings."
  (when (< (length args) 2)
    (error 'wardlisp-arity-error :message "let requires bindings and body"))
  (let ((bindings (first args))
        (body (rest args)))
    (unless (listp bindings)
      (error 'wardlisp-parse-error :message
             (format nil "let: bindings must be a list, got ~s" bindings)))
    (dolist (binding bindings)
      (unless (and (listp binding) (= 2 (length binding)))
        (error 'wardlisp-parse-error :message
               (format nil "let: each binding must be (name expr), got ~s" binding)))
      (check-not-boolean "let" (first binding))
      (unless (stringp (first binding))
        (error 'wardlisp-parse-error :message
               (format nil "let: binding name must be a symbol, got ~s"
                       (first binding)))))
    (let* ((names (mapcar #'first bindings))
           (vals (mapcar (lambda (b) (wardlisp-eval (second b) env ctx)) bindings))
           (new-env (env-extend env names vals)))
      (eval-body-forms body new-env ctx))))

(defun eval-let* (args env ctx)
  "Evaluate a let* form with sequential bindings."
  (when (< (length args) 2)
    (error 'wardlisp-arity-error :message "let* requires bindings and body"))
  (let ((bindings (first args))
        (body (rest args))
        (current-env env))
    (unless (listp bindings)
      (error 'wardlisp-parse-error :message
             (format nil "let*: bindings must be a list, got ~s" bindings)))
    (dolist (binding bindings)
      (unless (and (listp binding) (= 2 (length binding)))
        (error 'wardlisp-parse-error :message
               (format nil "let*: each binding must be (name expr), got ~s" binding)))
      (check-not-boolean "let*" (first binding))
      (unless (stringp (first binding))
        (error 'wardlisp-parse-error :message
               (format nil "let*: binding name must be a symbol, got ~s"
                       (first binding))))
      (let ((val (wardlisp-eval (second binding) current-env ctx)))
        (setf current-env
              (env-extend current-env (list (first binding)) (list val)))))
    (eval-body-forms body current-env ctx)))

(defun eval-lambda (args env)
  "Evaluate a lambda form, creating a closure."
  (when (< (length args) 2)
    (error 'wardlisp-arity-error :message "lambda requires params and body"))
  (let ((params (first args))
        (body (rest args)))
    (unless (listp params)
      (error 'wardlisp-parse-error :message
             (format nil "lambda: params must be a list, got ~s" params)))
    (dolist (p params)
      (check-not-boolean "lambda" p)
      (unless (stringp p)
        (error 'wardlisp-parse-error :message
               (format nil "lambda: param must be a symbol, got ~s" p))))
    (let ((seen nil))
      (dolist (p params)
        (when (member p seen :test #'string=)
          (error 'wardlisp-parse-error :message
                 (format nil "lambda: duplicate parameter name: ~a" p)))
        (push p seen)))
    (make-closure params body env)))

(defun validate-define-target (target context)
  "Validate a define target and return its name."
  (cond
    ((consp target)
     (let ((name (first target))
           (params (rest target)))
       (check-not-boolean context name)
       (unless (stringp name)
         (error 'wardlisp-parse-error :message
                (format nil "~a: function name must be a symbol, got ~s"
                        context name)))
       (dolist (p params)
         (check-not-boolean context p)
         (unless (stringp p)
           (error 'wardlisp-parse-error :message
                  (format nil "~a: parameter must be a symbol, got ~s"
                          context p))))
       (let ((seen nil))
         (dolist (p params)
           (when (member p seen :test #'string=)
             (error 'wardlisp-parse-error :message
                    (format nil "~a: duplicate parameter name: ~a" context p)))
           (push p seen)))
       name))
    ((stringp target)
     (check-not-boolean context target)
     target)
    (t
     (check-not-boolean context target)
     (error 'wardlisp-parse-error :message
            (format nil "Invalid define target: ~s" target)))))

(defun top-level-update-or-append (env name value)
  "Update an existing user binding or add to the user frame.
The first frame is the user frame; the rest are builtins (protected)."
  (let ((user-frame (first env)))
    (let ((pair (assoc name user-frame :test #'string=)))
      (if pair
          (setf (cdr pair) value)
          (setf (first env) (cons (cons name value) user-frame)))))
  value)

(defun eval-top-level-define (args env ctx)
  "Handle top-level define forms."
  (when (null args)
    (error 'wardlisp-arity-error :message
           "define: expected (define name value) or (define (name params...) body)"))
  (let ((target (first args)))
    (cond
      ((consp target)
       (let ((name (validate-define-target target "define"))
             (params (rest target))
             (body (rest args)))
         (when (null body)
           (error 'wardlisp-arity-error :message
                  "define: function form requires a body"))
         (let* ((closure (make-closure params body env name))
                (new-env (env-extend env (list name) (list closure))))
           (setf (closure-env closure) new-env)
           (top-level-update-or-append env name closure)
           closure)))
      ((stringp target)
       (validate-define-target target "define")
       (unless (= (length args) 2)
         (error 'wardlisp-arity-error :message
                (format nil "define: expected (define name value), got ~d argument~:p"
                        (length args))))
       (let ((value (wardlisp-eval (second args) env ctx)))
         (top-level-update-or-append env target value)
         value))
      (t
       (validate-define-target target "define")))))

(defun split-leading-defines (body)
  "Split BODY into leading define forms and remaining expressions.
Signals parse-error if a define appears after the first non-define expression."
  (let ((defs nil)
        (exprs nil)
        (seen-expr nil))
    (dolist (form body)
      (if (define-form-p form)
          (if seen-expr
              (error 'wardlisp-parse-error
                     :message "define must appear before expressions in a body")
              (push form defs))
          (progn
            (setf seen-expr t)
            (push form exprs))))
    (values (nreverse defs) (nreverse exprs))))

(defun ensure-distinct-define-names (defs)
  "Reject duplicate names in a single internal define block."
  (let ((seen nil))
    (dolist (form defs)
      (let* ((target (second form))
             (name (validate-define-target target "define")))
        (when (member name seen :test #'string=)
          (error 'wardlisp-parse-error
                 :message (format nil "duplicate define name in body: ~a" name)))
        (push name seen)))))

(defun eval-body-forms (body env ctx)
  "Evaluate BODY in a definition context.
Leading define forms are processed with letrec* semantics."
  (multiple-value-bind (defs exprs) (split-leading-defines body)
    (when (null exprs)
      (error 'wardlisp-parse-error
             :message "body must contain at least one expression after definitions"))
    (if (null defs)
        (eval-sequence exprs env ctx)
        (progn
          (ensure-distinct-define-names defs)
          (let* ((names (mapcar (lambda (form)
                                  (validate-define-target (second form) "define"))
                                defs))
                 (placeholders (make-list (length names)
                                          :initial-element +uninitialized-binding+))
                 (local-env (env-extend env names placeholders)))
            (dolist (form defs)
              (eval-local-define form local-env ctx))
            (eval-sequence exprs local-env ctx))))))

(defun eval-sequence (forms env ctx)
  "Evaluate FORMS sequentially, leaving the last form in tail position."
  (let ((result nil))
    (loop for (expr . rest) on forms
          do (setf result
                   (if rest
                       (wardlisp-eval expr env ctx)
                       (eval-inner expr env ctx))))
    result))

(defun eval-local-define (form env ctx)
  "Evaluate one leading body define in ENV."
  (let* ((args (cdr form))
         (target (first args)))
    (cond
      ((consp target)
       (let ((name (validate-define-target target "define"))
             (params (rest target))
             (body (rest args)))
         (when (null body)
           (error 'wardlisp-arity-error :message
                  "define: function form requires a body"))
         (let ((closure (make-closure params body env name)))
           (env-set! env name closure)
           closure)))
      ((stringp target)
       (validate-define-target target "define")
       (unless (= (length args) 2)
         (error 'wardlisp-arity-error :message
                (format nil "define: expected (define name value), got ~d argument~:p"
                        (length args))))
       (let ((value (wardlisp-eval (second args) env ctx)))
         (env-set! env target value)
         value))
      (t
       (validate-define-target target "define")))))

(defun reject-immediate-define-forms (forms context)
  "Reject direct define forms in a non-definition context."
  (dolist (form forms)
    (when (define-form-p form)
      (error 'wardlisp-parse-error
             :message (format nil "define is not allowed directly in ~a" context)))))

(defun eval-begin (args env ctx)
  "Evaluate a begin form."
  (reject-immediate-define-forms args "begin")
  (if (null args)
      nil
      (eval-sequence args env ctx)))

(defun eval-cond (clauses env ctx)
  "Evaluate a cond form.
Result expressions are in tail position (use eval-inner)."
  (dolist (clause clauses nil)
    (unless (listp clause)
      (error 'wardlisp-parse-error :message
             (format nil "cond: each clause must be a list, got ~s" clause)))
    (reject-immediate-define-forms (rest clause) "cond")
    (let ((test-result (wardlisp-eval (first clause) env ctx)))
      (when test-result
        (return
          (if (rest clause)
              (eval-sequence (rest clause) env ctx)
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
         (make-tail-call :expr (closure-body func) :env call-env :kind :body))))
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
