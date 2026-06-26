(defpackage :wardlisp/tests/string-test
  (:use :cl :rove :wardlisp :wardlisp/src/types))
(in-package :wardlisp/tests/string-test)

;;;; Acceptance tests for the string type (recurya novel-engine requirements
;;;; R1-R5). These exercise the public API (evaluate / print-value) so the host
;;;; contract is what is verified.

;;; --- R1.1 / R1.3 / R1.4: literal reads, self-evaluates, prints quoted ---

(deftest test-string-literal-self-evaluates-and-prints
  "A string literal evaluates to itself and prints back as \"...\"."
  (multiple-value-bind (result metrics) (evaluate "\"おはよう、元気？\"")
    (ok (null (getf metrics :error-type)))
    (ok (string= "\"おはよう、元気？\"" (print-value result)))))

(deftest test-string-literal-escapes-round-trip
  "Escapes \\\" \\\\ \\n \\t are read and restored by print-value."
  (let* ((bs (string #\\))
         ;; wardlisp source:  "a\"b\\c\nd\te"
         (src (concatenate 'string "\"a" bs "\"b" bs bs "c" bs "nd" bs "te\"")))
    (multiple-value-bind (result metrics) (evaluate src)
      (ok (null (getf metrics :error-type)))
      (ok (string= src (print-value result))))))

(deftest test-string-distinct-from-symbol-when-printed
  "A symbol prints bare; a string with the same characters prints quoted."
  (let ((sym (evaluate "'hello"))
        (str (evaluate "\"hello\"")))
    (ok (string= "hello" (print-value sym)))
    (ok (string= "\"hello\"" (print-value str)))))

;;; --- R1.5: equality compares string content; strings differ from symbols ---

(deftest test-string-equality
  (ok (eq t (evaluate "(equal? \"yo\" \"yo\")")))
  (ok (null (evaluate "(equal? \"yo\" \"yon\")")))
  ;; a string and a like-named symbol are never equal
  (ok (null (evaluate "(equal? \"foo\" 'foo)")))
  ;; nested in lists
  (ok (eq t (evaluate "(equal? (list \"a\" \"b\") (list \"a\" \"b\"))")))
  (ok (null (evaluate "(equal? (list \"a\" \"b\") (list \"a\" \"c\"))"))))

(deftest test-string-eq
  "eq? also compares string content (consistent with its symbol handling)."
  (ok (eq t (evaluate "(eq? \"yo\" \"yo\")")))
  (ok (null (evaluate "(eq? \"yo\" \"yon\")")))
  (ok (null (evaluate "(eq? \"foo\" 'foo)"))))

;;; --- R2: minimal string operations ---

(deftest test-string-append
  "Acceptance: (string-append \"好感度: \" (number->string 3)) => \"好感度: 3\"."
  (ok (string= "\"好感度: 3\""
               (print-value
                (evaluate "(string-append \"好感度: \" (number->string 3))"))))
  (ok (string= "\"\"" (print-value (evaluate "(string-append)"))))
  (ok (string= "\"abc\""
               (print-value (evaluate "(string-append \"a\" \"b\" \"c\")")))))

(deftest test-number-to-string
  (ok (string= "\"42\"" (print-value (evaluate "(number->string 42)"))))
  (ok (string= "\"-7\"" (print-value (evaluate "(number->string -7)"))))
  (ok (string= "\"3.14\"" (print-value (evaluate "(number->string 3.14)")))))

(deftest test-string-length
  (ok (= 5 (evaluate "(string-length \"hello\")")))
  (ok (= 0 (evaluate "(string-length \"\")")))
  ;; counts characters, not bytes
  (ok (= 3 (evaluate "(string-length \"あいう\")"))))

(deftest test-string-builtins-type-errors
  "String ops reject non-string / non-number arguments with a type error."
  (multiple-value-bind (r m) (evaluate "(string-append \"a\" 1)")
    (declare (ignore r))
    (ok (eq 'wardlisp-type-error (getf m :error-type))))
  (multiple-value-bind (r m) (evaluate "(number->string \"a\")")
    (declare (ignore r))
    (ok (eq 'wardlisp-type-error (getf m :error-type))))
  (multiple-value-bind (r m) (evaluate "(string-length 5)")
    (declare (ignore r))
    (ok (eq 'wardlisp-type-error (getf m :error-type))))
  ;; a symbol is not a string
  (multiple-value-bind (r m) (evaluate "(string-length 'foo)")
    (declare (ignore r))
    (ok (eq 'wardlisp-type-error (getf m :error-type)))))

;;; --- R1.6 / R5: string creation is charged against max-cons ---

(deftest test-string-literal-charged-against-max-cons
  "A literal longer than max-cons fails with a memory-limit error."
  (multiple-value-bind (r m) (evaluate "\"abcdefghij\"" :max-cons 5)
    (declare (ignore r))
    (ok (eq 'wardlisp-memory-limit-exceeded (getf m :error-type)))))

(deftest test-string-append-charged-against-max-cons
  (multiple-value-bind (r m) (evaluate "(string-append \"aaa\" \"bbb\")" :max-cons 3)
    (declare (ignore r))
    (ok (eq 'wardlisp-memory-limit-exceeded (getf m :error-type)))))

(deftest test-number-to-string-charged-against-max-cons
  (multiple-value-bind (r m) (evaluate "(number->string 123456)" :max-cons 3)
    (declare (ignore r))
    (ok (eq 'wardlisp-memory-limit-exceeded (getf m :error-type)))))

(deftest test-quoted-string-charged-against-max-cons
  "Strings inside quoted data are charged too."
  (multiple-value-bind (r m) (evaluate "(quote (\"aaaa\"))" :max-cons 2)
    (declare (ignore r))
    (ok (eq 'wardlisp-memory-limit-exceeded (getf m :error-type)))))

(deftest test-string-loop-generation-halts
  "Acceptance: a loop generating an ever-growing string halts on max-cons."
  (multiple-value-bind (r m)
      (evaluate "
        (define (rep n acc)
          (if (= n 0) acc (rep (- n 1) (string-append acc \"xxxx\"))))
        (rep 100000 \"\")"
                :fuel 100000000 :max-cons 10000)
    (declare (ignore r))
    (ok (eq 'wardlisp-memory-limit-exceeded (getf m :error-type)))))

(deftest test-small-string-allocates-proportional-cons
  "A small string under budget succeeds and reports cons proportional to length."
  (multiple-value-bind (r m) (evaluate "(string-append \"abc\" \"de\")")
    (ok (string= "\"abcde\"" (print-value r)))
    (ok (>= (getf m :cons-allocated) 5))))

;;; --- R4: host injects reader flags via :bindings ---

(deftest test-bindings-inject-value
  (ok (= 3 (evaluate "x" :bindings '(("x" . 3))))))

(deftest test-bindings-flag-controls-branch
  "Acceptance: an injected flag is referenced from a scene expression."
  (ok (= 1 (evaluate "(if met-alice 1 0)" :bindings '(("met-alice" . t)))))
  (ok (= 0 (evaluate "(if met-alice 1 0)" :bindings '(("met-alice" . nil))))))

(deftest test-bindings-multiple-and-arith
  (ok (= 4 (evaluate "(+ affection 1)" :bindings '(("affection" . 3))))))

(deftest test-bindings-string-value
  "A host can inject a boxed string via make-string-value."
  (ok (= 2 (evaluate "(string-length greeting)"
                     :bindings (list (cons "greeting"
                                           (make-string-value "hi")))))))

(deftest test-bindings-names-are-downcased
  "Binding names are downcased to match reader symbol normalization."
  (ok (eq t (evaluate "flag" :bindings '(("FLAG" . t))))))

(deftest test-bindings-define-overrides
  "A top-level define may shadow an injected binding."
  (ok (= 10 (evaluate "(define x 10) x" :bindings '(("x" . 1))))))

(deftest test-bindings-validation
  "Malformed :bindings are rejected with a type error."
  ;; not an alist
  (multiple-value-bind (r m) (evaluate "x" :bindings 42)
    (declare (ignore r))
    (ok (eq 'wardlisp-type-error (getf m :error-type))))
  ;; entry is not a pair
  (multiple-value-bind (r m) (evaluate "x" :bindings '(5))
    (declare (ignore r))
    (ok (eq 'wardlisp-type-error (getf m :error-type))))
  ;; non-string name
  (multiple-value-bind (r m) (evaluate "x" :bindings '((5 . 3)))
    (declare (ignore r))
    (ok (eq 'wardlisp-type-error (getf m :error-type))))
  ;; reserved name
  (multiple-value-bind (r m) (evaluate "x" :bindings '(("t" . 3)))
    (declare (ignore r))
    (ok (eq 'wardlisp-type-error (getf m :error-type)))))

(deftest test-bindings-rejects-compound-values
  "Only atoms may be injected; compound (ocons) values are rejected. The public
API exports no ocons constructor, and R4 only needs flags/counters, so an
injected pair (whose contents are unvalidated) is a type error."
  (multiple-value-bind (r m)
      (evaluate "x" :bindings (list (cons "x" (make-ocons 1 nil))))
    (declare (ignore r))
    (ok (eq 'wardlisp-type-error (getf m :error-type)))))
