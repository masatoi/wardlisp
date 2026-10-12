;;;; specs/generators.lisp
;;;;
;;;; Value and program generators shared by the contracts, and the predicates
;;;; that describe EVALUATE's documented vocabulary (docs/language-spec.md).
;;;; cl-spec binds *RANDOM-STATE* from the run's seed, so CL:RANDOM here
;;;; replays under the same seed.

(in-package :wardlisp/specs)

;;; --- Documented vocabulary ---

(defparameter *documented-error-types*
  '(wardlisp/src/types:wardlisp-parse-error
    wardlisp/src/types:wardlisp-name-error
    wardlisp/src/types:wardlisp-type-error
    wardlisp/src/types:wardlisp-arity-error
    wardlisp/src/types:wardlisp-step-limit-exceeded
    wardlisp/src/types:wardlisp-recursion-limit-exceeded
    wardlisp/src/types:wardlisp-memory-limit-exceeded
    wardlisp/src/types:wardlisp-integer-limit-exceeded
    wardlisp/src/types:wardlisp-output-limit-exceeded
    wardlisp/src/types:wardlisp-timeout-exceeded
    wardlisp/src/types:wardlisp-internal-error)
  "The error types of docs/language-spec.md section 12, as EVALUATE reports them
in the metrics' :ERROR-TYPE.")

(defparameter *metrics-keys*
  '(:steps-used :max-depth-reached :cons-allocated :output :fuel-remaining
    :error-type :error-message)
  "The keys of EVALUATE's metrics plist (docs/language-spec.md section 13.2).")

(defun non-negative-integer-p (value)
  "True when VALUE is an integer of at least zero."
  (and (integerp value) (not (minusp value))))

(defun metrics-plist-p (metrics)
  "True when METRICS is the plist of docs/language-spec.md section 13.2: each of
the seven keys exactly once, no other key, and each value of its documented
type."
  (and (listp metrics)
       (eql (list-length metrics) (* 2 (length *metrics-keys*)))
       (let ((keys (loop for key in metrics by #'cddr collect key)))
         (and (null (set-exclusive-or keys *metrics-keys*))
              (= (length keys) (length (remove-duplicates keys)))))
       (non-negative-integer-p (getf metrics :steps-used))
       (non-negative-integer-p (getf metrics :max-depth-reached))
       (non-negative-integer-p (getf metrics :cons-allocated))
       (stringp (getf metrics :output))
       (integerp (getf metrics :fuel-remaining))
       (symbolp (getf metrics :error-type))
       (typep (getf metrics :error-message) '(or null string))))

(defun proper-ocons-list-p (value)
  "True when VALUE is NIL or a chain of ocons cells ending in NIL."
  (loop for tail = value then (ocons-ocdr tail)
        repeat 100000
        do (cond ((null tail) (return t))
                 ((not (ocons-p tail)) (return nil)))))

(defun ocons-elements (value)
  "Return the elements of the proper ocons list VALUE as a CL list."
  (loop for tail = value then (ocons-ocdr tail)
        while (ocons-p tail)
        collect (ocons-ocar tail)))

(defun wardlisp-datum-p (value)
  "True when VALUE is a wardlisp data value: a number, T, NIL, a symbol (a bare
CL string), a boxed string, or a proper list of data values."
  (or (numberp value)
      (eq value t)
      (null value)
      (stringp value)
      (wstring-p value)
      (and (ocons-p value)
           (proper-ocons-list-p value)
           (every #'wardlisp-datum-p (ocons-elements value)))))

(defun datum-numbers (value)
  "Return every number reachable from the data VALUE."
  (cond ((numberp value) (list value))
        ((ocons-p value) (mapcan #'datum-numbers (ocons-elements value)))
        (t nil)))

;;; --- Random data ---

(defparameter *symbol-initials* "abcdefghijklmnopqrstuvwxyz"
  "Characters a generated symbol name starts with, so it never reads as a number.")

(defparameter *symbol-constituents* "abcdefghijklmnopqrstuvwxyz0123456789-?!*<>="
  "Characters a generated symbol name continues with.")

(defparameter *string-characters*
  (coerce (list #\a #\b #\z #\Space #\" #\\ #\Newline #\Tab #\( #\) #\; #\' #\#
                #\: (code-char #x3042) (code-char #x3093))
          'string)
  "Characters of generated string contents: plain text, every escape the reader
knows, the reader's own delimiters, and non-ASCII text.")

(defun random-element (sequence)
  "Return a random element of the non-empty SEQUENCE."
  (elt sequence (random (length sequence))))

(defun random-symbol-name ()
  "Return a symbol name the reader reads back as itself: lower case, starting
with a letter, and never the reserved t or nil."
  (loop for name = (coerce (cons (random-element *symbol-initials*)
                                 (loop repeat (random 6)
                                       collect (random-element *symbol-constituents*)))
                           'string)
        unless (member name '("t" "nil") :test #'string=)
          return name))

(defun random-text (&optional (max-length 8))
  "Return a CL string of at most MAX-LENGTH characters of *STRING-CHARACTERS*."
  (coerce (loop repeat (random (1+ max-length))
                collect (random-element *string-characters*))
          'string))

(defun random-integer ()
  "Return an integer, small most of the time and up to 2^60 in magnitude."
  (ecase (random 3)
    (0 (- (random 21) 10))
    (1 (- (random 2000001) 1000000))
    (2 (- (random (expt 2 61)) (expt 2 60)))))

(defun random-float ()
  "Return a double-float of at most 1000 in magnitude with three decimals, so
its printed form stays well inside the reader's 100-character literal limit."
  (/ (- (random 2000001) 1000000) 1000d0))

(defun wardlisp-list (items)
  "Return the proper ocons list of ITEMS."
  (reduce (lambda (item tail) (make-ocons item tail)) items
          :from-end t :initial-value nil))

(defun random-datum (&key (depth 2) (floats t))
  "Return a random wardlisp data value nested at most DEPTH lists deep.  With
FLOATS false no float is generated."
  (ecase (random (if (plusp depth) 8 7))
    (0 (random-integer))
    (1 (if floats (random-float) (random-integer)))
    ((2 3) (random-symbol-name))
    (4 (make-string-value (random-text)))
    (5 t)
    (6 nil)
    (7 (wardlisp-list (loop repeat (random 4)
                            collect (random-datum :depth (1- depth) :floats floats))))))

(defun copy-datum (value)
  "Return a structurally equal copy of the data VALUE that shares no ocons cell,
boxed string or symbol string with it."
  (cond ((ocons-p value) (wardlisp-list (mapcar #'copy-datum (ocons-elements value))))
        ((wstring-p value) (make-string-value (wstring-value value)))
        ((stringp value) (copy-seq value))
        (t value)))

(defun datum-source (value)
  "Return source text that evaluates to the data VALUE: a quoted printed form
for symbols and lists, the printed form itself for self-evaluating atoms."
  (if (or (stringp value) (ocons-p value))
      (format nil "'~A" (print-value value))
      (print-value value)))

;;; --- Random programs ---

(defparameter *program-templates*
  '("(define (count n) (if (= n 0) 0 (+ 1 (count (- n 1))))) (count ~D)"
    "(define (build n acc) (if (= n 0) acc (build (- n 1) (cons n acc))))
(length (build ~D nil))"
    "(define (shout n) (if (= n 0) 'done (begin (print n) (shout (- n 1)))))
(shout ~D)"
    "(define (echo xs) (if (null? xs) 'done (begin (print (car xs)) (echo (cdr xs)))))
(echo (list ~D \"hi\" 'sym (list 1 \"a\\nb\")))"
    "(define (grow n s) (if (= n 0) (string-length s) (grow (- n 1) (string-append s \"ab\"))))
(grow ~D \"\")"
    "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact ~D)"
    "(define (spin n) (spin n)) (spin ~D)"
    "(let* ((n ~D) (xs (list n (random 10) (random 100)))) (apply + xs))"
    "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib ~D)"
    "(define (square-up x n) (if (= n 0) x (square-up (* x x) (- n 1))))
(square-up 1.5e10 ~D)")
  "Deterministic programs that use recursion, allocation, output, big numbers
and non-termination; each takes one integer.")

(defparameter *expression-operators*
  '("+" "-" "*" "/" "quotient" "mod" "=" "<" ">=" "list" "cons" "car" "cdr"
    "append" "length" "null?" "atom?" "not" "eq?" "equal?" "print"
    "string-append" "string-length" "number->string" "integer?" "random")
  "Builtins random expressions apply, with arbitrary arguments.")

(defparameter *soup-tokens*
  '("(" ")" "(" ")" "'" "\"" "\"s\"" "define" "lambda" "let" "if" "cond" "f" "x"
    "1" "-3" "2.5" "#" ":" ";" "\\" "nil" "t" "print" "+" "car" "cons" "." "0"
    "1e400" "-1.5e309" ".5e400" "1e-400" "1e308" "99999999999999999999999"
    "#.(quit)" "cl:car")
  "Tokens, broken pieces and forbidden syntax that token soup is made of.")

(defun random-template-program ()
  "Return one of *PROGRAM-TEMPLATES* applied to a random integer."
  (format nil (random-element *program-templates*) (random 60)))

(defun random-expression (depth)
  "Return the text of a random expression nested at most DEPTH applications deep."
  (if (or (zerop depth) (zerop (random 3)))
      (datum-source (random-datum :depth 1))
      (format nil "(~A~{ ~A~})" (random-element *expression-operators*)
              (loop repeat (random 4) collect (random-expression (1- depth))))))

(defun random-token-soup ()
  "Return random tokens joined by spaces; usually not a well-formed program."
  (format nil "~{~A~^ ~}" (loop repeat (random 12) collect (random-element *soup-tokens*))))

(defun random-program ()
  "Return program text: a template program, a random expression or token soup."
  (ecase (random 3)
    (0 (random-template-program))
    (1 (random-expression 3))
    (2 (random-token-soup))))

(defun deterministic-program ()
  "Return program text whose run depends on nothing but the text: a template
program without RANDOM, or a random expression without RANDOM."
  (loop for program = (if (zerop (random 2))
                          (random-template-program)
                          (random-expression 3))
        unless (search "random" program)
          return program))

(defun resource-program ()
  "Return a template program without RANDOM over a small argument, so that it
usually terminates and uses some steps, cells, call depth and output."
  (loop for template = (random-element *program-templates*)
        unless (search "random" template)
          return (format nil template (1+ (random 20)))))

;;; --- Generators and named specs ---

(defgenerator datum ()
  "A wardlisp data value, floats included."
  (random-datum))

(defspec wardlisp-datum (satisfies wardlisp-datum-p) (:generator datum))

(defgenerator datum-without-floats ()
  "A wardlisp data value without floats, whose printed form decides equality."
  (random-datum :floats nil))

(defspec exact-datum (satisfies wardlisp-datum-p) (:generator datum-without-floats))

(defgenerator datum-pair ()
  "Two data values: half the time a value and a structural copy of it, else two
values drawn independently."
  (let ((first (random-datum :floats nil)))
    (list first (if (zerop (random 2)) (copy-datum first) (random-datum :floats nil)))))

(defspec exact-datum-pair (satisfies consp) (:generator datum-pair))

(defgenerator program ()
  "Program text: well-formed, ill-formed or forbidden."
  (random-program))

(defspec program-text string (:generator program))

(defgenerator terminating-program ()
  "Program text whose result and metrics depend on nothing but the text."
  (deterministic-program))

(defspec deterministic-program-text string (:generator terminating-program))

(defgenerator limit-program ()
  "Program text that usually terminates while using every counted resource."
  (resource-program))

(defspec resource-program-text string (:generator limit-program))

(defgenerator symbol-text ()
  "A symbol name the reader reads back as itself."
  (random-symbol-name))

(defspec wardlisp-symbol-name string (:generator symbol-text))

(defgenerator host-text ()
  "A CL string of generated string contents."
  (random-text 12))

(defspec host-string string (:generator host-text))

(defgenerator wide-integer-value ()
  "An integer whose magnitude is log-uniform up to 2^66, so values on both sides
of the default max-integer, 2^64, are drawn."
  (let ((bits (random 67)))
    (- (random (expt 2 (1+ bits))) (expt 2 bits))))

(defspec wide-integer integer (:generator wide-integer-value))

(defgenerator out-of-range-float-text ()
  "A float literal beyond double-float range: a nonzero mantissa and a decimal
exponent from 309 to 2000, of either sign."
  (format nil "~:[~;-~]~D.~3,'0De~D" (zerop (random 2)) (1+ (random 9)) (random 1000)
          (+ 309 (random 1692))))

(defspec out-of-range-float-literal string (:generator out-of-range-float-text))

(defgenerator wide-float-text ()
  "A float literal with a nonzero mantissa and a decimal exponent from -300 to
300, written with an exponent so it stays short."
  (format nil "~:[~;-~]~D.~3,'0De~D" (zerop (random 2)) (1+ (random 9)) (random 1000)
          (- (random 601) 300)))

(defspec wide-float-literal string (:generator wide-float-text))

(defgenerator evaluate-arguments ()
  "A whole EVALUATE call: a program and every resource limit, small enough that
each limit is reached by some generated program."
  (list (random-program)
        :fuel (1+ (random 5000))
        :max-depth (1+ (random 60))
        :max-cons (1+ (random 500))
        :max-output (1+ (random 100))
        ;; A quarter of the calls allow numbers beyond double-float range, where
        ;; float arithmetic overflows before max-integer is reached.
        :max-integer (if (zerop (random 4))
                         (expt 2 (+ 1024 (random 2000)))
                         (expt 2 (+ 8 (random 60))))
        :random-seed (random 1000)))
