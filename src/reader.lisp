(defpackage :wardlisp/src/reader
  (:use :cl :wardlisp/src/types)
  (:export #:wardlisp-read #:wardlisp-read-program #:+max-parse-depth+
           #:+max-string-length+))
(in-package :wardlisp/src/reader)

(defconstant +max-parse-depth+ 1000
  "Maximum nesting depth for S-expression parsing.")

(defconstant +max-string-length+ 10000
  "Maximum number of characters allowed in a single string literal.")

(defun wardlisp-read (input)
  "Read a single expression from INPUT string."
  (multiple-value-bind (expr pos) (read-expr input 0)
    (declare (ignore pos))
    expr))

(defun wardlisp-read-program (input)
  "Read all top-level expressions from INPUT string. Returns a list."
  (let ((exprs '())
        (pos 0)
        (len (length input)))
    (loop
      (setf pos (skip-whitespace-and-comments input pos))
      (when (>= pos len) (return (nreverse exprs)))
      (multiple-value-bind (expr new-pos) (read-expr input pos)
        (push expr exprs)
        (setf pos new-pos)))))

(defun read-expr (input pos &optional (depth 0))
  "Parse one expression starting at POS. Returns (values expr new-pos)."
  (setf pos (skip-whitespace-and-comments input pos))
  (when (>= pos (length input))
    (error 'wardlisp-parse-error :message "Unexpected end of input"))
  (let ((ch (char input pos)))
    (cond ((char= ch #\() (read-list input (1+ pos) (1+ depth)))
          ((char= ch #\))
           (error 'wardlisp-parse-error :message
                  (format nil "Unexpected closing parenthesis at ~a"
                          (pos-to-location input pos))))
          ;; A quote nests its datum one level deeper, so it counts towards
          ;; the nesting limit like a list does.
          ((char= ch #\') (read-quote input (1+ pos) (1+ depth)))
          ((char= ch #\#)
           (error 'wardlisp-parse-error :message
                  (format nil "Reader macros (#) are not allowed at ~a"
                          (pos-to-location input pos))))
          ((char= ch #\") (read-string input (1+ pos)))
          (t (read-atom input pos)))))

(defun read-list (input pos &optional (depth 0))
  "Parse list body after opening paren. Returns (values list new-pos)."
  (when (> depth +max-parse-depth+)
    (error 'wardlisp-parse-error :message
           (format nil "Nesting depth ~d exceeds limit ~d" depth
                   +max-parse-depth+)))
  (let ((elements 'nil)
        (open-pos pos))
    (loop (setf pos (skip-whitespace-and-comments input pos))
          (when (>= pos (length input))
            (error 'wardlisp-parse-error :message
                   (format nil "Unterminated list starting at ~a"
                           (pos-to-location input (1- open-pos)))))
          (when (char= (char input pos) #\))
            (return (values (nreverse elements) (1+ pos))))
          (multiple-value-bind (expr new-pos)
              (read-expr input pos depth)
            (push expr elements)
            (setf pos new-pos)))))

(defun read-quote (input pos &optional (depth 0))
  "Parse quoted expression after quote character.  DEPTH counts the quote as a
nesting level, so '''...x cannot recurse past the nesting limit."
  (when (> depth +max-parse-depth+)
    (error 'wardlisp-parse-error :message
           (format nil "Nesting depth ~d exceeds limit ~d" depth
                   +max-parse-depth+)))
  (multiple-value-bind (expr new-pos) (read-expr input pos depth)
    (values (list "quote" expr) new-pos)))

(defun read-string (input pos)
  "Parse a string literal. POS points just past the opening double-quote.
Returns (values wstring new-pos). Supports escapes \\\" \\\\ \\n \\t and caps
the decoded length at +max-string-length+."
  (let ((out (make-string-output-stream))
        (len (length input))
        (open (1- pos))
        (count 0))
    (loop
      (when (>= pos len)
        (error 'wardlisp-parse-error :message
               (format nil "Unterminated string starting at ~a"
                       (pos-to-location input open))))
      (let ((ch (char input pos)))
        (cond
          ((char= ch #\")
           (return (values (make-wstring (get-output-stream-string out))
                           (1+ pos))))
          ((char= ch #\\)
           (incf pos)
           (when (>= pos len)
             (error 'wardlisp-parse-error :message
                    (format nil "Unterminated string escape at ~a"
                            (pos-to-location input open))))
           (let ((esc (char input pos)))
             (write-char
              (cond ((char= esc #\") #\")
                    ((char= esc #\\) #\\)
                    ((char= esc #\n) #\Newline)
                    ((char= esc #\t) #\Tab)
                    (t (error 'wardlisp-parse-error :message
                              (format nil "Invalid string escape \\~a at ~a"
                                      esc (pos-to-location input pos)))))
              out)
             (incf pos)
             (incf count)))
          (t (write-char ch out)
             (incf pos)
             (incf count)))
        (when (> count +max-string-length+)
          (error 'wardlisp-parse-error :message
                 (format nil "String literal too long (max ~d characters)"
                         +max-string-length+)))))))

(defun read-atom (input pos)
  "Parse an atom (integer or symbol) starting at POS."
  (let ((start pos) (len (length input)))
    (loop while (and (< pos len) (atom-char-p (char input pos)))
          do (incf pos))
    (when (= start pos)
      (error 'wardlisp-parse-error :message
             (format nil "Unexpected character: ~a at ~a"
                     (char input start) (pos-to-location input start))))
    (let ((token (subseq input start pos)))
      (when (find #\: token)
        (error 'wardlisp-parse-error :message
               (format nil "Package prefix not allowed: ~a at ~a"
                       (abbreviate-text token) (pos-to-location input start))))
      (values (parse-token token) pos))))

(defun parse-token (token)
  "Convert a token string to an AST value."
  (let ((lower (string-downcase token)))
    (cond ((string= lower "t") t)
          ((string= lower "nil") nil)
          ((integer-token-p token)
           (when (> (length token) 100)
             (error 'wardlisp-parse-error
                    :message (format nil "Integer literal too long (~d digits, max 100)"
                                    (length token))))
           (parse-integer token :radix 10))
          ((float-token-p token)
           (when (> (length token) 100)
             (error 'wardlisp-parse-error
                    :message (format nil "Float literal too long (~d chars, max 100)"
                                    (length token))))
           (parse-float-token token))
          (t lower))))

(defun parse-float-token (token)
  "Read TOKEN, which FLOAT-TOKEN-P accepted, as the double-float nearest to the
decimal number it writes, ties to even -- in the subnormal range too, where the
host's conversion truncates.  The host reader is not used, so no host setting
can change the result.  A literal beyond double-float range is a
wardlisp-parse-error; one too small for any double rounds to zero, keeping its
sign."
  (multiple-value-bind (negative mantissa exponent) (decimal-token-parts token)
    (let ((magnitude (decimal-to-double mantissa exponent token)))
      (if negative (- magnitude) magnitude))))

(defun decimal-token-parts (token)
  "Split the float TOKEN into a sign, an integer mantissa made of all its digits,
and the decimal exponent that mantissa needs: the value is
mantissa * 10^exponent."
  (let* ((negative (char= #\- (char token 0)))
         (start (if (find (char token 0) "+-") 1 0))
         (marker (position-if (lambda (ch) (char-equal ch #\e)) token))
         (number-part (subseq token start (or marker (length token))))
         (point (position #\. number-part))
         (fraction-length (if point (- (length number-part) point 1) 0))
         (digits (remove #\. number-part))
         (exponent (if marker (parse-integer token :start (1+ marker)) 0)))
    (values negative
            (if (string= digits "") 0 (parse-integer digits))
            (- exponent fraction-length))))

(defun decimal-to-double (mantissa exponent token)
  "Return the double-float nearest to MANTISSA * 10^EXPONENT (a non-negative
value), ties to even.  Signals wardlisp-parse-error, naming TOKEN, when it is
beyond double-float range."
  (flet ((out-of-range ()
           (error 'wardlisp-parse-error
                  :message (format nil "Float literal out of range: ~a" token))))
    (when (zerop mantissa)
      (return-from decimal-to-double 0.0d0))
    ;; The decimal order of magnitude bounds the work before any exact
    ;; arithmetic: 1e309 is beyond the range, 1e-400 below half the smallest
    ;; subnormal.
    (let ((magnitude (+ exponent (length (princ-to-string mantissa)) -1)))
      (cond ((> magnitude 309) (out-of-range))
            ((< magnitude -400) (return-from decimal-to-double 0.0d0))))
    (or (rational-to-double (* mantissa (expt 10 exponent)))
        (out-of-range))))

(defun ascii-digit-p (ch)
  "Is CH one of the ASCII digits 0-9?  Other Unicode decimal digits are ordinary
symbol characters (section 15 of docs/language-spec.md)."
  (char<= #\0 ch #\9))

(defun integer-token-p (token)
  "Check if TOKEN looks like an integer."
  (let ((start (if (and (> (length token) 1)
                        (or (char= (char token 0) #\-) (char= (char token 0) #\+)))
                   1
                   0)))
    (and (> (length token) start) (every #'ascii-digit-p (subseq token start)))))

(defun float-token-p (token)
  "Check if TOKEN looks like a floating-point number (e.g., 3.14, -0.5, .5, 1e3, 2.5e-10)."
  (let* ((len (length token))
         (start (if (and (> len 1)
                         (or (char= (char token 0) #\-)
                             (char= (char token 0) #\+)))
                    1
                    0)))
    (when (= start len) (return-from float-token-p nil))
    (let ((has-dot nil)
          (has-e nil)
          (has-digit nil)
          (i start))
      (loop while (< i len)
            for ch = (char token i)
            do (cond
                 ((ascii-digit-p ch)
                  (setf has-digit t)
                  (incf i))
                 ((and (char= ch #\.) (not has-dot) (not has-e))
                  (setf has-dot t)
                  (incf i))
                 ((and (or (char= ch #\e) (char= ch #\E))
                       (not has-e) has-digit)
                  (setf has-e t)
                  (setf has-digit nil)
                  (incf i)
                  (when (and (< i len)
                             (or (char= (char token i) #\+)
                                 (char= (char token i) #\-)))
                    (incf i)))
                 (t (return-from float-token-p nil))))
      (and has-digit (or has-dot has-e)))))

(defun atom-char-p (ch)
  "Is CH a valid atom character for wardlisp?"
  (and (not (char= ch #\())
       (not (char= ch #\)))
       (not (char= ch #\'))
       (not (char= ch #\;))
       (not (char= ch #\#))
       (not (char= ch #\"))
       (not (char= ch #\`))
       (not (char= ch #\,))
       (not (char= ch #\\))
       (not (char= ch #\|))
       (not (whitespace-p ch))))

(defun whitespace-p (ch)
  "Is CH a whitespace character?"
  (or (char= ch #\Space)
      (char= ch #\Tab)
      (char= ch #\Newline)
      (char= ch #\Return)))

(defun skip-whitespace-and-comments (input pos)
  "Skip whitespace and semicolon comments starting at POS."
  (let ((len (length input)))
    (loop
      (when (>= pos len) (return pos))
      (cond
        ((whitespace-p (char input pos))
         (incf pos))
        ((char= (char input pos) #\;)
         (loop while (and (< pos len)
                          (char/= (char input pos) #\Newline))
               do (incf pos)))
        (t (return pos))))))

(defun pos-to-location (input pos)
  "Convert character position to line:column string."
  (let ((line 1) (col 1))
    (loop for i below (min pos (length input))
          do (if (char= (char input i) #\Newline)
                 (progn (incf line) (setf col 1))
                 (incf col)))
    (format nil "~d:~d" line col)))
