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
