;;; SICP Chapter 3: Modularity, Objects, and State
;;; Adapted for wardlisp: purely functional style (no set!, no mutation)
;;;
;;; Key adaptation strategy:
;;; - State is threaded explicitly through function arguments and return values
;;; - "Objects" return new state instead of mutating internal state
;;; - Streams implemented as thunks (delayed evaluation via closures)

;;; ============================================================
;;; 3.1 Local State — Functional Style
;;; ============================================================

;;; 3.1.1 Bank Account (functional)
;;; Instead of mutable balance, return new balance with each operation

(define (make-account balance)
  (list 'account balance))

(define (account-balance account)
  (car (cdr account)))

(define (account-withdraw account amount)
  (let ((balance (account-balance account)))
    (if (>= balance amount)
        (list 'ok (make-account (- balance amount)))
        (list 'insufficient-funds account))))

(define (account-deposit account amount)
  (let ((balance (account-balance account)))
    (list 'ok (make-account (+ balance amount)))))

;;; Usage: thread state through operations
(define (account-demo)
  (let ((acc (make-account 100)))
    (let ((r1 (account-withdraw acc 25)))
      (let ((acc2 (car (cdr r1))))
        (let ((r2 (account-withdraw acc2 50)))
          (let ((acc3 (car (cdr r2))))
            (account-balance acc3)))))))

(account-demo)  ;=> 25

;;; 3.1.2 Accumulator (functional)
;;; Returns pair of (current-sum . accumulator-function)
(define (make-accumulator initial)
  (lambda (amount)
    (+ initial amount)))

;;; To chain calls, compose the results
(define (accumulate-all amounts initial)
  (if (null? amounts)
      initial
      (accumulate-all (cdr amounts) (+ initial (car amounts)))))

(accumulate-all '(5 10 15) 0)  ;=> 30

;;; 3.1.3 Monte Carlo (functional)
;;; Estimate probability by counting passing trials
;;; Uses a simple linear congruential pseudo-random generator

(define (rand-update x)
  (mod (+ (* 1103515245 x) 12345) 1000000))

(define (monte-carlo trials experiment seed)
  (define (iter remaining passed seed)
    (if (= remaining 0)
        (list passed trials)
        (let ((new-seed (rand-update seed)))
          (if (experiment new-seed)
              (iter (- remaining 1) (+ passed 1) new-seed)
              (iter (- remaining 1) passed new-seed)))))
  (iter trials 0 seed))

;;; Estimate probability that gcd(random, random) = 1
;;; (approximates 6/pi^2 ~ 0.608)
(define (cesaro-test seed)
  (let ((r1 (mod seed 97))
        (r2 (mod (rand-update seed) 97)))
    (= (gcd (+ r1 1) (+ r2 1)) 1)))

(define (gcd a b)
  (if (= b 0) a (gcd b (mod a b))))

(monte-carlo 50 cesaro-test 42)
;=> (passed . total) — ratio approximates 6/pi^2

;;; ============================================================
;;; 3.2 The Environment Model — Demonstrated via Closures
;;; ============================================================

;;; Closures capture their defining environment

(define (make-counter initial)
  (lambda (msg)
    (cond ((eq? msg 'value) initial)
          ((eq? msg 'increment)
           (make-counter (+ initial 1)))
          ((eq? msg 'decrement)
           (make-counter (- initial 1)))
          (t nil))))

(define c0 (make-counter 0))
(define c1 (c0 'increment))
(define c2 (c1 'increment))
(define c3 (c2 'increment))

(c0 'value)  ;=> 0
(c3 'value)  ;=> 3

;;; Nested environments via closure chain
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (list (- balance amount)
              (make-withdraw (- balance amount)))
        (list 'insufficient balance))))

(define w0 (make-withdraw 100))
(define r1 (w0 25))
(car r1)  ;=> 75

;;; ============================================================
;;; 3.3 Modeling with Mutable Data — Functional Alternatives
;;; ============================================================

;;; 3.3.1 Functional Pairs (no set-car!/set-cdr! needed)
;;; All "mutation" creates new structures

;;; 3.3.2 Functional Queue
;;; Queue = (front-list . rear-list)
;;; Amortized O(1) operations

(define (make-queue) (cons nil nil))

(define (queue-empty? q) (null? (car q)))

(define (queue-front q)
  (if (queue-empty? q)
      nil
      (car (car q))))

(define (queue-insert q item)
  (cons (car q) (cons item (cdr q))))

(define (queue-normalize q)
  (if (null? (car q))
      (cons (my-reverse (cdr q)) nil)
      q))

(define (my-reverse lst)
  (define (iter lst acc)
    (if (null? lst)
        acc
        (iter (cdr lst) (cons (car lst) acc))))
  (iter lst nil))

(define (queue-delete q)
  (if (queue-empty? q)
      q
      (queue-normalize (cons (cdr (car q)) (cdr q)))))

;;; Queue usage
(define (queue-demo)
  (let ((q (make-queue)))
    (let ((q (queue-insert q 1)))
      (let ((q (queue-insert q 2)))
        (let ((q (queue-insert q 3)))
          (let ((q (queue-normalize q)))
            (let ((front (queue-front q)))
              (let ((q (queue-delete q)))
                (list front (queue-front q))))))))))

(queue-demo)  ;=> (1 2)

;;; 3.3.3 Functional Table (Association List)

(define (make-table) nil)

(define (table-lookup key table)
  (cond ((null? table) nil)
        ((eq? key (car (car table)))
         (cdr (car table)))
        (t (table-lookup key (cdr table)))))

(define (table-insert key value table)
  (define (replace-or-add entries)
    (cond ((null? entries)
           (list (cons key value)))
          ((eq? key (car (car entries)))
           (cons (cons key value) (cdr entries)))
          (t (cons (car entries)
                   (replace-or-add (cdr entries))))))
  (replace-or-add table))

;;; Two-dimensional table
(define (make-2d-table) nil)

(define (table-2d-lookup key1 key2 table)
  (let ((subtable (table-lookup key1 table)))
    (if (null? subtable)
        nil
        (table-lookup key2 subtable))))

(define (table-2d-insert key1 key2 value table)
  (let ((subtable (table-lookup key1 table)))
    (if (null? subtable)
        (table-insert key1
                      (table-insert key2 value (make-table))
                      table)
        (table-insert key1
                      (table-insert key2 value subtable)
                      table))))

;;; Table usage
(define (table-demo)
  (let ((tbl (make-table)))
    (let ((tbl (table-insert 'a 1 tbl)))
      (let ((tbl (table-insert 'b 2 tbl)))
        (let ((tbl (table-insert 'a 10 tbl)))
          (list (table-lookup 'a tbl)
                (table-lookup 'b tbl)))))))

(table-demo)  ;=> (10 2)

(define (table-2d-demo)
  (let ((tbl (make-2d-table)))
    (let ((tbl (table-2d-insert 'math 'plus 43 tbl)))
      (let ((tbl (table-2d-insert 'math 'minus 45 tbl)))
        (let ((tbl (table-2d-insert 'letters 'a 97 tbl)))
          (list (table-2d-lookup 'math 'plus tbl)
                (table-2d-lookup 'letters 'a tbl)))))))

(table-2d-demo)  ;=> (43 97)

;;; 3.3.4 Digital Circuit Simulator (Functional)
;;; Represent wire signals as lists of values over time
;;; Gate functions produce output signal lists from input signal lists

(define (logical-not s)
  (if (= s 0) 1 0))

(define (logical-and s1 s2)
  (if (and (= s1 1) (= s2 1)) 1 0))

(define (logical-or s1 s2)
  (if (or (= s1 1) (= s2 1)) 1 0))

;;; Half adder: purely functional
(define (half-adder a b)
  (let ((d (logical-or a b))
        (c (logical-and a b)))
    (let ((e (logical-not c)))
      (let ((sum (logical-and d e)))
        (list sum c)))))

(half-adder 0 0)  ;=> (0 0) = sum=0, carry=0
(half-adder 1 0)  ;=> (1 0)
(half-adder 0 1)  ;=> (1 0)
(half-adder 1 1)  ;=> (0 1) = sum=0, carry=1

;;; Full adder
(define (full-adder a b c-in)
  (let ((h1 (half-adder b c-in)))
    (let ((h2 (half-adder a (car h1))))
      (list (car h2)
            (logical-or (car (cdr h1)) (car (cdr h2)))))))

(full-adder 1 1 0)  ;=> (0 1)
(full-adder 1 1 1)  ;=> (1 1)

;;; Ripple-carry adder: add two n-bit numbers represented as lists
(define (ripple-carry-add a-bits b-bits)
  (define (iter as bs carry result)
    (if (null? as)
        (cons carry result)
        (let ((sum-carry (full-adder (car as) (car bs) carry)))
          (iter (cdr as) (cdr bs)
                (car (cdr sum-carry))
                (cons (car sum-carry) result)))))
  (iter (my-reverse a-bits) (my-reverse b-bits) 0 nil))

;;; 5 (101) + 3 (011) = 8 (1000)
(ripple-carry-add '(1 0 1) '(0 1 1))  ;=> (1 0 0 0)
;;; 7 (111) + 1 (001) = 8 (1000)
(ripple-carry-add '(1 1 1) '(0 0 1))  ;=> (1 0 0 0)

;;; ============================================================
;;; 3.4 Concurrency — Functional Approach
;;; ============================================================

;;; In wardlisp there is no concurrency, but we can demonstrate
;;; the key insight: pure functions are inherently thread-safe
;;; because they have no shared mutable state.

;;; Serialized account access is unnecessary when state is explicit.
;;; Two "concurrent" withdrawals on the same account must be
;;; explicitly sequenced since each returns a new account.

(define (safe-transfer from-acc to-acc amount)
  (let ((result (account-withdraw from-acc amount)))
    (if (eq? (car result) 'ok)
        (let ((new-from (car (cdr result))))
          (let ((dep-result (account-deposit to-acc amount)))
            (list 'ok new-from (car (cdr dep-result)))))
        (list 'failed from-acc to-acc))))

(define (transfer-demo)
  (let ((a1 (make-account 100))
        (a2 (make-account 50)))
    (let ((result (safe-transfer a1 a2 30)))
      (list (account-balance (car (cdr result)))
            (account-balance (car (cdr (cdr result))))))))

(transfer-demo)  ;=> (70 80)

;;; ============================================================
;;; 3.5 Streams — Lazy Evaluation via Closures
;;; ============================================================

;;; Streams are represented as (value . thunk)
;;; where thunk is (lambda () rest-of-stream)
;;; Empty stream is nil

(define (stream-cons x thunk)
  (cons x thunk))

(define (stream-car s) (car s))

(define (stream-cdr s)
  (if (null? s) nil ((cdr s))))

(define (stream-null? s) (null? s))

;;; 3.5.1 Streams Are Delayed Lists

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      nil
      (stream-cons
       (proc (stream-car s))
       (lambda () (stream-map proc (stream-cdr s))))))

(define (stream-filter pred s)
  (cond ((stream-null? s) nil)
        ((pred (stream-car s))
         (stream-cons
          (stream-car s)
          (lambda () (stream-filter pred (stream-cdr s)))))
        (t (stream-filter pred (stream-cdr s)))))

(define (stream-for-each proc s n)
  (if (or (stream-null? s) (= n 0))
      nil
      (begin
        (proc (stream-car s))
        (stream-for-each proc (stream-cdr s) (- n 1)))))

(define (stream-take s n)
  (if (or (= n 0) (stream-null? s))
      nil
      (cons (stream-car s)
            (stream-take (stream-cdr s) (- n 1)))))

;;; Stream enumeration
(define (stream-enumerate-interval low high)
  (if (> low high)
      nil
      (stream-cons
       low
       (lambda () (stream-enumerate-interval (+ low 1) high)))))

(stream-take (stream-enumerate-interval 1 20) 5)  ;=> (1 2 3 4 5)

;;; 3.5.2 Infinite Streams

(define (integers-from n)
  (stream-cons n (lambda () (integers-from (+ n 1)))))

(define integers (integers-from 1))

(stream-take integers 10)  ;=> (1 2 3 4 5 6 7 8 9 10)

;;; Sieve of Eratosthenes
(define (divisible? x y) (= (mod x y) 0))

(define (sieve s)
  (stream-cons
   (stream-car s)
   (lambda ()
     (sieve
      (stream-filter
       (lambda (x) (not (divisible? x (stream-car s))))
       (stream-cdr s))))))

(define primes (sieve (integers-from 2)))

(stream-take primes 10)  ;=> (2 3 5 7 11 13 17 19 23 29)

;;; No-sevens: integers not divisible by 7
(define (no-sevens)
  (stream-filter
   (lambda (x) (not (divisible? x 7)))
   integers))

(stream-take (no-sevens) 10)  ;=> (1 2 3 4 5 6 8 9 10 11)

;;; 3.5.2 Defining Streams Implicitly

;;; Ones: infinite stream of 1s
(define (ones) (stream-cons 1 ones))

(stream-take (ones) 5)  ;=> (1 1 1 1 1)

;;; Stream zip with operation
(define (stream-zip-with op s1 s2)
  (if (or (stream-null? s1) (stream-null? s2))
      nil
      (stream-cons
       (op (stream-car s1) (stream-car s2))
       (lambda ()
         (stream-zip-with op (stream-cdr s1) (stream-cdr s2))))))

;;; Fibonacci stream
(define (fibs-from a b)
  (stream-cons a (lambda () (fibs-from b (+ a b)))))

(define fibs (fibs-from 0 1))

(stream-take fibs 12)  ;=> (0 1 1 2 3 5 8 13 21 34 55 89)

;;; Partial sums stream
(define (partial-sums s)
  (define (helper s acc)
    (if (stream-null? s)
        nil
        (let ((new-acc (+ acc (stream-car s))))
          (stream-cons new-acc
                       (lambda () (helper (stream-cdr s) new-acc))))))
  (helper s 0))

(stream-take (partial-sums integers) 8)  ;=> (1 3 6 10 15 21 28 36)

;;; 3.5.3 Exploiting the Stream Paradigm

;;; Square root via stream of improving approximations (Newton's method)
(define (sqrt-stream n)
  (define (improve guess)
    (/ (+ guess (/ n guess)) 2.0))
  (define (sqrt-iter guess)
    (stream-cons guess
                 (lambda () (sqrt-iter (improve guess)))))
  (sqrt-iter 1.0))

(stream-take (sqrt-stream 2.0) 6)
;=> (1.0 1.5 1.4166... 1.41421... ...) converging to sqrt(2)

;;; Interleave two streams
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (stream-cons
       (stream-car s1)
       (lambda () (interleave s2 (stream-cdr s1))))))

(stream-take (interleave integers (stream-map (lambda (x) (- x)) integers)) 10)
;=> (1 -1 2 -2 3 -3 4 -4 5 -5)

;;; Scale a stream
(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor)) s))

(stream-take (scale-stream integers 3) 5)  ;=> (3 6 9 12 15)

;;; Powers of 2
(define (powers-of-two)
  (define (iter n)
    (stream-cons n (lambda () (iter (* n 2)))))
  (iter 1))

(stream-take (powers-of-two) 10)  ;=> (1 2 4 8 16 32 64 128 256 512)

;;; 3.5.4 Streams and Delayed Evaluation

;;; Integral approximation (integer-scaled, using trapezoidal rule)
;;; Computes sum of stream values (scaled integration)
(define (stream-integral s initial dt-scale)
  (define (iter s acc)
    (if (stream-null? s)
        nil
        (let ((new-acc (+ acc (* (stream-car s) dt-scale))))
          (stream-cons new-acc
                       (lambda () (iter (stream-cdr s) new-acc))))))
  (iter s initial))

;;; Accumulated sum of integers: 1, 1+2, 1+2+3, ...
(define (triangular-numbers)
  (stream-integral integers 0 1))

(stream-take (triangular-numbers) 8)  ;=> (1 3 6 10 15 21 28 36)

;;; 3.5.5 Modularity of Functional Programs

;;; Random number stream (linear congruential generator)
(define (rand-stream seed)
  (let ((next (rand-update seed)))
    (stream-cons next
                 (lambda () (rand-stream next)))))

(define (rand-update x)
  (mod (+ (* 1103515245 x) 12345) 1000000))

;;; Cesaro stream experiment
;;; Generate pairs of random numbers, check if gcd = 1
(define (cesaro-stream seed)
  (let ((randoms (rand-stream seed)))
    (define (iter s)
      (let ((r1 (stream-car s))
            (r2 (stream-car (stream-cdr s))))
        (stream-cons
         (if (= (gcd (+ (mod r1 97) 1)
                      (+ (mod r2 97) 1)) 1)
             1
             0)
         (lambda () (iter (stream-cdr (stream-cdr s)))))))
    (iter randoms)))

;;; Running count of successes
(define (running-ratio success-stream)
  (define (iter s total passed)
    (if (stream-null? s)
        nil
        (let ((new-total (+ total 1))
              (new-passed (+ passed (stream-car s))))
          (stream-cons
           (list new-passed new-total)
           (lambda () (iter (stream-cdr s) new-total new-passed))))))
  (iter success-stream 0 0))

;;; Take some samples
(stream-take (running-ratio (cesaro-stream 42)) 5)
;=> list of (passed total) pairs showing convergence
