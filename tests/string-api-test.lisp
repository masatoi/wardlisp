(defpackage :wardlisp/tests/string-api-test
  (:use :cl :rove :wardlisp))
(in-package :wardlisp/tests/string-api-test)

;;;; R3: the host classifies and walks an EVALUATE result using ONLY the
;;;; :wardlisp package -- no dependency on internal source packages. This
;;;; package deliberately does not :use wardlisp/src/types.

(deftest test-result-walk-classifies-symbol-string-number
  "A (say \"やあ\" 3) result is walked and each node is classified via public API."
  (multiple-value-bind (result metrics) (evaluate "(list 'say \"やあ\" 3)")
    (ok (null (getf metrics :error-type)))
    (ok (ocons-p result))
    (let* ((tag  (ocons-ocar result))
           (rest (ocons-ocdr result))
           (text (ocons-ocar rest))
           (num  (ocons-ocar (ocons-ocdr rest))))
      ;; tag is a symbol
      (ok (symbol-value-p tag))
      (ok (not (string-value-p tag)))
      (ok (not (number-value-p tag)))
      (ok (string= "say" tag))
      ;; text is a string
      (ok (string-value-p text))
      (ok (not (symbol-value-p text)))
      (ok (not (number-value-p text)))
      (ok (string= "やあ" (string-value text)))
      ;; num is a number
      (ok (number-value-p num))
      (ok (not (string-value-p num)))
      (ok (not (symbol-value-p num)))
      (ok (= 3 num)))))

(deftest test-string-value-rejects-non-strings
  (ok (signals (string-value 3) 'wardlisp-type-error))
  ;; a symbol is a bare CL string, not a boxed string
  (ok (signals (string-value (evaluate "'say")) 'wardlisp-type-error)))

(deftest test-make-string-value-round-trip
  (let ((s (make-string-value "hi")))
    (ok (string-value-p s))
    (ok (string= "hi" (string-value s)))
    (ok (string= "\"hi\"" (print-value s))))
  (ok (signals (make-string-value 42) 'wardlisp-type-error)))

(deftest test-string-value-returns-independent-copy
  "string-value returns a fresh copy; mutating it must not corrupt the wstring
\(symmetric with make-string-value, which copies on the way in)."
  (let* ((s (make-string-value "hi"))
         (extracted (string-value s)))
    (setf (char extracted 0) #\X)
    (ok (string= "hi" (string-value s)))))
