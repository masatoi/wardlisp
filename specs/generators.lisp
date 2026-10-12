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

(defun wardlisp-number-p (value)
  "True when VALUE is a wardlisp number: an integer or a finite double-float
(sections 3.1 and 3.2).  Ratios, complexes, other floats, infinities and NaNs
are not wardlisp values."
  (or (integerp value)
      (and (typep value 'double-float)
           (not (sb-ext:float-infinity-p value))
           (not (sb-ext:float-nan-p value)))))

(defun wardlisp-datum-p (value)
  "True when VALUE is a wardlisp data value: a number, T, NIL, a symbol (a bare
CL string), a boxed string, or a proper list of data values."
  (or (wardlisp-number-p value)
      (eq value t)
      (null value)
      (stringp value)
      (wstring-p value)
      (and (ocons-p value)
           (proper-ocons-list-p value)
           (every #'wardlisp-datum-p (ocons-elements value)))))

(defun wardlisp-value-p (value)
  "True when VALUE is a value a wardlisp program can produce (section 3): a
number, T, NIL, a symbol, a boxed string, a closure, a builtin, or a pair whose
car and cdr are such values.  No other host object may come out of EVALUATE."
  (let ((pending (list value)))
    (loop while pending
          do (let ((item (pop pending)))
               (cond ((ocons-p item)
                      (push (ocons-ocar item) pending)
                      (push (ocons-ocdr item) pending))
                     ((not (or (wardlisp-number-p item) (eq item t) (null item)
                               (stringp item) (wstring-p item)
                               (closure-p item) (builtin-p item)))
                      (return-from wardlisp-value-p nil)))))
    t))

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

(defun random-wide-float ()
  "Return a nonzero double-float whose decimal exponent is uniform from -300 to
300, of either sign."
  (* (if (zerop (random 2)) 1 -1)
     (+ 1 (random 9d0))
     (expt 10d0 (- (random 601) 300))))

(defun wardlisp-list (items)
  "Return the proper ocons list of ITEMS."
  (reduce (lambda (item tail) (make-ocons item tail)) items
          :from-end t :initial-value nil))

(defun random-datum (&key (depth 2) (floats t) wide-floats)
  "Return a random wardlisp data value nested at most DEPTH lists deep.  With
FLOATS false no float is generated; with WIDE-FLOATS floats range over
magnitudes from 1e-300 to 1e300 instead of staying within 1000."
  (ecase (random (if (plusp depth) 8 7))
    (0 (random-integer))
    (1 (cond ((not floats) (random-integer))
             (wide-floats (random-wide-float))
             (t (random-float))))
    ((2 3) (random-symbol-name))
    (4 (make-string-value (random-text)))
    (5 t)
    (6 nil)
    (7 (wardlisp-list (loop repeat (random 4)
                            collect (random-datum :depth (1- depth) :floats floats
                                                  :wide-floats wide-floats))))))

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

(defparameter *named-templates*
  '((:count . "(define (count n) (if (= n 0) 0 (+ 1 (count (- n 1))))) (count ~D)")
    (:build . "(define (build n acc) (if (= n 0) acc (build (- n 1) (cons n acc))))
(length (build ~D nil))")
    (:shout . "(define (shout n) (if (= n 0) 'done (begin (print n) (shout (- n 1)))))
(shout ~D)")
    (:echo . "(define (echo xs) (if (null? xs) 'done (begin (print (car xs)) (echo (cdr xs)))))
(echo (list ~D \"hi\" 'sym (list 1 \"a\\nb\")))")
    (:grow . "(define (grow n s)
  (if (= n 0) (string-length s) (grow (- n 1) (string-append s \"ab\"))))
(grow ~D \"\")")
    (:fact . "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact ~D)")
    (:spin . "(define (spin n) (spin n)) (spin ~D)")
    (:dice . "(let* ((n ~D) (xs (list n (random 10) (random 100)))) (apply + xs))")
    (:fib . "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib ~D)")
    (:square-up . "(define (square-up x n) (if (= n 0) x (square-up (* x x) (- n 1))))
(square-up 1.5e10 ~D)"))
  "Programs that use recursion, allocation, output, big numbers, randomness and
non-termination, by name; each takes one integer.")

(defparameter *program-templates* (mapcar #'cdr *named-templates*)
  "The templates of *NAMED-TEMPLATES*.")

(defparameter *limit-programs*
  '((:steps-used (:count 1 20) (:build 2 20) (:shout 1 20) (:echo 1 20) (:grow 1 20)
     (:fact 1 20) (:fib 2 10))
    (:cons-allocated (:build 2 20) (:grow 1 20) (:echo 1 20))
    (:max-depth-reached (:count 1 20) (:fact 1 20) (:fib 2 10))
    (:output (:shout 1 20) (:echo 1 20)))
  "For each resource, (TEMPLATE LOW HIGH) entries whose programs succeed with
fuel 20000 and the default limits for every argument from LOW to HIGH, and use
at least two units of that resource.")

(defparameter *expression-operators*
  '("+" "-" "*" "/" "quotient" "mod" "=" "<" ">=" "list" "cons" "car" "cdr"
    "append" "length" "null?" "atom?" "not" "eq?" "equal?" "print"
    "string-append" "string-length" "number->string" "integer?" "random")
  "Builtins random expressions apply, with arbitrary arguments.")

(defparameter *soup-tokens*
  '("(" ")" "(" ")" "'" "\"" "\"s\"" "define" "lambda" "let" "if" "cond" "f" "x"
    "1" "-3" "2.5" "#" ":" ";" "\\" "nil" "t" "print" "+" "car" "cons" "." "0"
    "1e400" "-1.5e309" ".5e400" "1e-400" "1e308" "99999999999999999999999"
    "#.(quit)" "cl:car"
    ;; Unicode decimal digits are symbol characters, never digits.
    "１.５" "１２" "٣.٥" "２e3" "'１.５")
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

(defun limit-program (resource)
  "Return a program from *LIMIT-PROGRAMS* that succeeds and uses at least two
units of RESOURCE, so that the limit properties never hold vacuously."
  (destructuring-bind (name low high)
      (random-element (rest (assoc resource *limit-programs*)))
    (format nil (cdr (assoc name *named-templates*)) (+ low (random (1+ (- high low)))))))

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

(defgenerator step-hungry-program ()
  "A program that succeeds and uses at least two steps."
  (limit-program :steps-used))

(defspec step-using-program string (:generator step-hungry-program))

(defgenerator cons-hungry-program ()
  "A program that succeeds and allocates at least two cells."
  (limit-program :cons-allocated))

(defspec cons-using-program string (:generator cons-hungry-program))

(defgenerator depth-hungry-program ()
  "A program that succeeds and reaches call depth two or more."
  (limit-program :max-depth-reached))

(defspec depth-using-program string (:generator depth-hungry-program))

(defgenerator output-hungry-program ()
  "A program that succeeds and prints at least two characters."
  (limit-program :output))

(defspec output-using-program string (:generator output-hungry-program))

(defgenerator wide-datum ()
  "A wardlisp data value whose floats range over magnitudes from 1e-300 to
1e300, so their printed forms run from a few characters to hundreds."
  (random-datum :wide-floats t))

(defspec printable-datum (satisfies wardlisp-datum-p) (:generator wide-datum))

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

(defparameter *non-ascii-zeros* '(#xFF10 #x0660 #x06F0 #x0966 #x09E6 #x0E50)
  "Code points of the digit zero of some Unicode decimal digit sets: fullwidth,
Arabic-Indic, Extended Arabic-Indic, Devanagari, Bengali and Thai.")

(defgenerator non-ascii-number-text ()
  "A token shaped like an integer or a float literal whose digits all come from
one non-ASCII Unicode decimal digit set."
  (let ((zero (random-element *non-ascii-zeros*)))
    (flet ((digits (count)
             (coerce (loop repeat count collect (code-char (+ zero (random 10)))) 'string)))
      (ecase (random 3)
        (0 (digits (1+ (random 4))))
        (1 (format nil "~A.~A" (digits (1+ (random 3))) (digits (1+ (random 3)))))
        (2 (format nil "~Ae~A" (digits (1+ (random 2))) (digits 1)))))))

(defspec non-ascii-number-token string (:generator non-ascii-number-text))

(defun random-digits (count)
  "Return COUNT random ASCII digits as a string."
  (coerce (loop repeat count collect (code-char (+ 48 (random 10)))) 'string))

(defgenerator float-literal-parts ()
  "The parts of a float literal of any shape section 2.5.2 admits -- digits and
a point (\"12.\", \"12.5\", \".5\"), digits and an exponent (\"12e3\"), or both
-- as (TEXT SIGN INTEGER-DIGITS FRACTION-DIGITS EXPONENT), with an exponent
from -300 to 300 when there is one."
  (let* ((sign (random-element '("" "+" "-")))
         (shape (random 4))
         (integer-digits (if (= shape 2) "" (random-digits (1+ (random 4)))))
         (fraction-digits (case shape
                            (0 "")
                            ((1 2) (random-digits (1+ (random 4))))
                            (t "")))
         (point (if (= shape 3) "" "."))
         (exponent (if (or (= shape 3) (zerop (random 2))) (- (random 601) 300) nil)))
    (list (format nil "~A~A~A~A~@[e~D~]" sign integer-digits point fraction-digits exponent)
          sign integer-digits fraction-digits exponent)))

(defspec float-literal (satisfies consp) (:generator float-literal-parts))

(defparameter *documented-builtins*
  '(("+" . :any) ("-" . :some) ("*" . :any) ("quotient" . 2) ("mod" . 2) ("/" . 2)
    ("=" . 2) ("<" . 2) ("<=" . 2) (">" . 2) (">=" . 2)
    ("cons" . 2) ("car" . 1) ("cdr" . 1) ("list" . :any) ("null?" . 1) ("atom?" . 1)
    ("length" . 1) ("append" . :any) ("integer?" . 1) ("number?" . 1) ("not" . 1)
    ("eq?" . 2) ("equal?" . 2) ("print" . 1) ("random" . 1)
    ("string-append" . :any) ("number->string" . 1) ("string-length" . 1))
  "The builtins of sections 7 and 17.5 with their arity: a count, :ANY for zero
or more, :SOME for one or more.")

(defparameter *probe-sources*
  '("(lambda (x) x)" "car" "(cons 1 2)" "1.5e300" "-0.0" "0" "'()" "t" "\"\"")
  "Source text for values a random datum does not cover: a closure, a builtin, a
dotted pair, a huge float, negative zero, zero, the empty list, t and the empty
string.")

(defgenerator probe-arguments ()
  "Three argument source texts of any type."
  (loop repeat 3
        collect (if (zerop (random 3))
                    (random-element *probe-sources*)
                    (datum-source (random-datum :depth 1 :wide-floats t)))))

(defspec builtin-arguments (satisfies consp) (:generator probe-arguments))

(defun random-limit (typical)
  "Return a limit: usually from 1 to TYPICAL, so generated programs reach it, and
one time in eight a bignum, since the contract admits any positive integer."
  (if (zerop (random 8))
      (expt 10 (+ 19 (random 40)))
      (1+ (random typical))))

(defgenerator evaluate-arguments ()
  "A whole EVALUATE call: a program and every resource limit, small enough that
each limit is reached by some generated program, and now and then a bignum."
  (list (random-program)
        :fuel (1+ (random 5000))
        :max-depth (random-limit 60)
        :max-cons (random-limit 500)
        :max-output (random-limit 100)
        ;; Numbers beyond double-float range, where float arithmetic overflows
        ;; before max-integer is reached; ordinary ones; and ones so small that
        ;; lengths and literals exceed them.
        :max-integer (ecase (random 4)
                       (0 (expt 2 (+ 1024 (random 2000))))
                       ((1 2) (expt 2 (+ 8 (random 60))))
                       (3 (1+ (random 256))))
        :random-seed (random 1000)))
