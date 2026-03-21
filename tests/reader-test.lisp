(defpackage :omoikane-lisp/tests/reader-test
  (:use :cl :rove :omoikane-lisp/src/reader :omoikane-lisp/src/types))
(in-package :omoikane-lisp/tests/reader-test)

;; --- Atoms ---
(deftest test-read-integer
  (ok (= 42 (omoikane-read "42")))
  (ok (= -7 (omoikane-read "-7")))
  (ok (= 0 (omoikane-read "0"))))

(deftest test-read-boolean
  (ok (eq t (omoikane-read "t")))
  (ok (eq nil (omoikane-read "nil"))))

(deftest test-read-symbol
  (ok (equal "+" (omoikane-read "+")))
  (ok (equal "factorial" (omoikane-read "factorial")))
  (ok (equal "null?" (omoikane-read "null?"))))

;; --- Lists ---
(deftest test-read-list
  (ok (equal '("+" 1 2) (omoikane-read "(+ 1 2)")))
  (ok (equal nil (omoikane-read "()")))
  (ok (equal '("list" 1 2 3) (omoikane-read "(list 1 2 3)"))))

(deftest test-read-nested-list
  (ok (equal '("+" ("*" 2 3) 4) (omoikane-read "(+ (* 2 3) 4)"))))

;; --- Quote ---
(deftest test-read-quote
  (ok (equal '("quote" "x") (omoikane-read "'x")))
  (ok (equal '("quote" (1 2 3)) (omoikane-read "'(1 2 3)"))))

;; --- Whitespace & comments ---
(deftest test-read-whitespace
  (ok (= 42 (omoikane-read "  42  ")))
  (ok (equal '("+" 1 2) (omoikane-read " ( +  1  2 ) "))))

(deftest test-read-comment
  (ok (= 42 (omoikane-read "; this is a comment
42"))))

;; --- Multiple expressions ---
(deftest test-read-program
  (ok (equal '(("define" ("f" "x") ("+" "x" 1)) ("f" 10))
             (omoikane-read-program "(define (f x) (+ x 1))
(f 10)"))))

;; --- Errors ---
(deftest test-read-unmatched-paren
  (ok (signals (omoikane-read "(+ 1 2") 'omoikane-parse-error))
  (ok (signals (omoikane-read ")") 'omoikane-parse-error)))

(deftest test-read-reject-package-prefix
  (ok (signals (omoikane-read "cl:car") 'omoikane-parse-error)))

(deftest test-read-reject-reader-macro
  (ok (signals (omoikane-read "#.42") 'omoikane-parse-error))
  (ok (signals (omoikane-read "#'car") 'omoikane-parse-error)))
