(defpackage :wardlisp/tests/reader-test
  (:use :cl :rove :wardlisp/src/reader :wardlisp/src/types))
(in-package :wardlisp/tests/reader-test)

;; --- Atoms ---
(deftest test-read-integer
  (ok (= 42 (wardlisp-read "42")))
  (ok (= -7 (wardlisp-read "-7")))
  (ok (= 0 (wardlisp-read "0"))))

(deftest test-read-boolean
  (ok (eq t (wardlisp-read "t")))
  (ok (eq nil (wardlisp-read "nil"))))

(deftest test-read-symbol
  (ok (equal "+" (wardlisp-read "+")))
  (ok (equal "factorial" (wardlisp-read "factorial")))
  (ok (equal "null?" (wardlisp-read "null?"))))

;; --- Lists ---
(deftest test-read-list
  (ok (equal '("+" 1 2) (wardlisp-read "(+ 1 2)")))
  (ok (equal nil (wardlisp-read "()")))
  (ok (equal '("list" 1 2 3) (wardlisp-read "(list 1 2 3)"))))

(deftest test-read-nested-list
  (ok (equal '("+" ("*" 2 3) 4) (wardlisp-read "(+ (* 2 3) 4)"))))

;; --- Quote ---
(deftest test-read-quote
  (ok (equal '("quote" "x") (wardlisp-read "'x")))
  (ok (equal '("quote" (1 2 3)) (wardlisp-read "'(1 2 3)"))))

;; --- Whitespace & comments ---
(deftest test-read-whitespace
  (ok (= 42 (wardlisp-read "  42  ")))
  (ok (equal '("+" 1 2) (wardlisp-read " ( +  1  2 ) "))))

(deftest test-read-comment
  (ok (= 42 (wardlisp-read "; this is a comment
42"))))

;; --- Multiple expressions ---
(deftest test-read-program
  (ok (equal '(("define" ("f" "x") ("+" "x" 1)) ("f" 10))
             (wardlisp-read-program "(define (f x) (+ x 1))
(f 10)"))))

;; --- Errors ---
(deftest test-read-unmatched-paren
  (ok (signals (wardlisp-read "(+ 1 2") 'wardlisp-parse-error))
  (ok (signals (wardlisp-read ")") 'wardlisp-parse-error)))

(deftest test-read-reject-package-prefix
  (ok (signals (wardlisp-read "cl:car") 'wardlisp-parse-error)))

(deftest test-read-reject-reader-macro
  (ok (signals (wardlisp-read "#.42") 'wardlisp-parse-error))
  (ok (signals (wardlisp-read "#'car") 'wardlisp-parse-error)))

;;; --- Coverage: empty input ---
(deftest test-read-empty-input
  (ok (signals (wardlisp-read "") 'wardlisp-parse-error))
  (ok (signals (wardlisp-read "   ") 'wardlisp-parse-error))
  (ok (signals (wardlisp-read "; just a comment") 'wardlisp-parse-error)))

;;; --- Coverage: empty program ---
(deftest test-read-empty-program
  (ok (equal nil (wardlisp-read-program "")))
  (ok (equal nil (wardlisp-read-program "  ; only comments  "))))

;;; --- Coverage: integer with + prefix ---
(deftest test-read-positive-integer
  (ok (= 7 (wardlisp-read "+7"))))

;;; --- Coverage: whitespace variants ---
(deftest test-read-whitespace-variants
  ;; Tab
  (ok (= 42 (wardlisp-read (format nil "~c42" #\Tab))))
  ;; Carriage return
  (ok (= 42 (wardlisp-read (format nil "~c42" #\Return))))
  ;; Multiple comments
  (ok (= 1 (wardlisp-read "; comment 1
; comment 2
1"))))

;;; --- Coverage: case normalization ---
(deftest test-read-case-normalization
  (ok (equal "hello" (wardlisp-read "HELLO")))
  (ok (equal "mixed" (wardlisp-read "MiXeD"))))

(deftest test-read-float
  (ok (= 3.14d0 (wardlisp-read "3.14")))
  (ok (= -0.5d0 (wardlisp-read "-0.5")))
  (ok (= 0.5d0 (wardlisp-read ".5")))
  (ok (= 1000.0d0 (wardlisp-read "1e3")))
  (ok (= 2.5d-10 (wardlisp-read "2.5e-10")))
  (ok (= 100.0d0 (wardlisp-read "+100.0")))
  ;; Float in list
  (let ((result (wardlisp-read "(+ 1.5 2.5)")))
    (ok (= 1.5d0 (second result)))
    (ok (= 2.5d0 (third result)))))

(deftest test-numeric-literal-100-char-boundary
  "100-char integer is ok, 101-char integer is parse-error"
  ;; 100 digits (exactly at limit) — should succeed
  (let ((token-100 (make-string 100 :initial-element #\1)))
    (ok (integerp (wardlisp-read token-100))))
  ;; 101 digits — should fail
  (let ((token-101 (make-string 101 :initial-element #\1)))
    (ok (signals (wardlisp-read token-101)
         'wardlisp-parse-error)))
  ;; 100-char float is ok
  (let ((token-100f (concatenate 'string (make-string 98 :initial-element #\1) ".0")))
    (ok (numberp (wardlisp-read token-100f))))
  ;; 101-char float is parse-error
  (let ((token-101f (concatenate 'string (make-string 99 :initial-element #\1) ".0")))
    (ok (signals (wardlisp-read token-101f)
         'wardlisp-parse-error))))

(deftest test-out-of-range-float-literal-is-parse-error
  "A float literal beyond double-float range is a parse-error, not a host reader error"
  (dolist (token '("1e400" "-1e400" "1.5e309" "+2E999" ".5e400"))
    (ok (signals (wardlisp-read token) 'wardlisp-parse-error) token))
  (ok (signals (wardlisp-read-program "(+ 1 1e400)") 'wardlisp-parse-error))
  ;; The largest finite literal still reads
  (ok (floatp (wardlisp-read "1.7976931348623157e308"))))

(deftest test-non-ascii-digits-are-symbol-characters
  "Only ASCII 0-9 are digits (section 15); other Unicode digits make a symbol"
  (let ((fullwidth-float (format nil "~C.~C" (code-char #xFF11) (code-char #xFF15)))
        (fullwidth-integer (format nil "~C~C" (code-char #xFF11) (code-char #xFF12)))
        (arabic-indic (format nil "~C.~C" (code-char #x0663) (code-char #x0665))))
    (ok (equal fullwidth-float (wardlisp-read fullwidth-float)))
    (ok (equal fullwidth-integer (wardlisp-read fullwidth-integer)))
    (ok (equal arabic-indic (wardlisp-read arabic-indic)))
    (ok (equal (list "+" fullwidth-integer) (wardlisp-read (format nil "(+ ~A)" fullwidth-integer))))))

(deftest test-reader-ignores-host-reader-settings
  "Numeric literals read the same whatever the host's reader variables are"
  (let ((*read-base* 16)
        (*read-default-float-format* 'single-float)
        (*readtable* (copy-readtable nil)))
    (set-macro-character #\1 (lambda (stream char) (declare (ignore stream char)) :injected))
    (ok (eql 100000.0d0 (wardlisp-read "1e5")))
    (ok (eql 1.5d0 (wardlisp-read "1.5")))
    (ok (eql 10 (wardlisp-read "10")))
    (ok (signals (wardlisp-read "1e400") 'wardlisp-parse-error))))

(deftest test-float-literal-with-trailing-point
  "digit+ '.' is a float literal (section 15), not an integer or an error"
  (ok (eql 1.0d0 (wardlisp-read "1.")))
  (ok (eql -5.0d0 (wardlisp-read "-5.")))
  (ok (eql 5.0d0 (wardlisp-read "+5.")))
  (ok (eql 100000.0d0 (wardlisp-read "1.e5"))))

;;; --- String literals ---

(deftest test-read-string-basic
  (ok (wstring-p (wardlisp-read "\"hi\"")))
  (ok (string= "hi" (wstring-value (wardlisp-read "\"hi\""))))
  (ok (string= "" (wstring-value (wardlisp-read "\"\"")))))

(deftest test-read-string-escapes
  ;; \n -> newline, \t -> tab, \" -> quote, \\ -> backslash
  (ok (string= (format nil "a~cb" #\Newline)
               (wstring-value (wardlisp-read "\"a\\nb\""))))
  (ok (string= (format nil "a~cb" #\Tab)
               (wstring-value (wardlisp-read "\"a\\tb\""))))
  (ok (string= "a\"b" (wstring-value (wardlisp-read "\"a\\\"b\""))))
  (ok (string= "a\\b" (wstring-value (wardlisp-read "\"a\\\\b\"")))))

(deftest test-read-string-in-list
  (let ((result (wardlisp-read "(say \"hi\")")))
    (ok (equal "say" (first result)))
    (ok (wstring-p (second result)))
    (ok (string= "hi" (wstring-value (second result))))))

(deftest test-read-string-distinct-from-symbol
  ;; Same characters: a symbol is a bare CL string, a string is a wstring.
  (ok (stringp (wardlisp-read "hello")))
  (ok (not (wstring-p (wardlisp-read "hello"))))
  (ok (wstring-p (wardlisp-read "\"hello\""))))

(deftest test-read-string-unterminated
  (ok (signals (wardlisp-read "\"abc") 'wardlisp-parse-error))
  (ok (signals (wardlisp-read "\"abc\\") 'wardlisp-parse-error)))

(deftest test-read-string-invalid-escape
  (ok (signals (wardlisp-read "\"a\\xb\"") 'wardlisp-parse-error)))

(deftest test-read-string-length-cap
  "A string at the limit reads; one character over signals a parse-error."
  (let ((at-limit (format nil "\"~a\""
                          (make-string +max-string-length+ :initial-element #\a)))
        (over-limit (format nil "\"~a\""
                            (make-string (1+ +max-string-length+)
                                         :initial-element #\a))))
    (ok (wstring-p (wardlisp-read at-limit)))
    (ok (signals (wardlisp-read over-limit) 'wardlisp-parse-error))))
