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
