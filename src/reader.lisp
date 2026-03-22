(defpackage :wardlisp/src/reader
  (:use :cl :wardlisp/src/types)
  (:export #:wardlisp-read #:wardlisp-read-program #:+max-parse-depth+))
(in-package :wardlisp/src/reader)

(defconstant +max-parse-depth+ 1000
  "Maximum nesting depth for S-expression parsing.")

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
          ((char= ch #\') (read-quote input (1+ pos) depth))
          ((char= ch #\#)
           (error 'wardlisp-parse-error :message
                  (format nil "Reader macros (#) are not allowed at ~a"
                          (pos-to-location input pos))))
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
  "Parse quoted expression after quote character."
  (multiple-value-bind (expr new-pos) (read-expr input pos depth)
    (values (list "quote" expr) new-pos)))

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
                       token (pos-to-location input start))))
      (values (parse-token token) pos))))

(defun parse-token (token)
  "Convert a token string to an AST value."
  (let ((lower (string-downcase token)))
    (cond
      ((string= lower "t") t)
      ((string= lower "nil") nil)
      ((integer-token-p token)
       (when (> (length token) 100)
         (error 'wardlisp-parse-error
                :message (format nil "Integer literal too long (~d digits, max 100)"
                                 (length token))))
       (parse-integer token :radix 10))
      (t lower))))

(defun integer-token-p (token)
  "Check if TOKEN looks like an integer."
  (let ((start (if (and (> (length token) 1)
                        (or (char= (char token 0) #\-)
                            (char= (char token 0) #\+)))
                   1
                   0)))
    (and (> (length token) start)
         (every #'digit-char-p (subseq token start)))))

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
