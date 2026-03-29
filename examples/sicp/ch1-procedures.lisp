;;; SICP Chapter 1: Building Abstractions with Procedures
;;; Adapted for wardlisp (no mutation, no built-in map/filter)

;;; ============================================================
;;; 1.1 The Elements of Programming
;;; ============================================================

;;; 1.1.4 Compound Procedures

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(sum-of-squares 3 4)  ;=> 25

;;; 1.1.6 Conditional Expressions and Predicates

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        (t (- x))))

(abs -5)   ;=> 5
(abs 3)    ;=> 3

;;; Square root (Newton's method)
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (/ (+ guess (/ x guess)) 2.0))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(sqrt 9.0)    ;=> ~3.0
(sqrt 2.0)    ;=> ~1.4142
(sqrt 144.0)  ;=> ~12.0

;;; ============================================================
;;; 1.2 Procedures and the Processes They Generate
;;; ============================================================

;;; 1.2.1 Linear Recursion and Iteration

;;; Recursive factorial
(define (factorial-rec n)
  (if (= n 1)
      1
      (* n (factorial-rec (- n 1)))))

(factorial-rec 6)  ;=> 720

;;; Iterative factorial (tail-recursive)
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product) (+ counter 1))))
  (iter 1 1))

(factorial 10)  ;=> 3628800

;;; 1.2.2 Tree Recursion

;;; Fibonacci (tree-recursive)
(define (fib-tree n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (t (+ (fib-tree (- n 1))
              (fib-tree (- n 2))))))

(fib-tree 10)  ;=> 55

;;; Fibonacci (iterative)
(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

(fib 10)  ;=> 55
(fib 20)  ;=> 6765

;;; Counting change
(define (count-change amount)
  (define (first-denomination kinds)
    (cond ((= kinds 1) 1)
          ((= kinds 2) 5)
          ((= kinds 3) 10)
          ((= kinds 4) 25)
          ((= kinds 5) 50)))
  (define (cc amount kinds)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kinds 0)) 0)
          (t (+ (cc amount (- kinds 1))
                (cc (- amount (first-denomination kinds)) kinds)))))
  (cc amount 5))

(count-change 11)  ;=> 4
(count-change 15)  ;=> 6

;;; 1.2.4 Exponentiation

;;; Linear recursive
(define (expt-rec b n)
  (if (= n 0)
      1
      (* b (expt-rec b (- n 1)))))

(expt-rec 2 10)  ;=> 1024

;;; Iterative
(define (expt-iter b n)
  (define (iter b counter product)
    (if (= counter 0)
        product
        (iter b (- counter 1) (* b product))))
  (iter b n 1))

(expt-iter 2 10)  ;=> 1024

;;; Fast exponentiation (successive squaring)
(define (even? n) (= (mod n 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (quotient n 2))))
        (t (* b (fast-expt b (- n 1))))))

(fast-expt 2 16)  ;=> 65536
(fast-expt 3 5)   ;=> 243

;;; 1.2.5 Greatest Common Divisors (Euclid's algorithm)
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (mod a b))))

(gcd 206 40)  ;=> 2
(gcd 48 36)   ;=> 12

;;; 1.2.6 Primality Testing

;;; Trial division
(define (smallest-divisor n)
  (define (divides? a b) (= (mod b a) 0))
  (define (find-divisor n test)
    (cond ((> (square test) n) n)
          ((divides? test n) test)
          (t (find-divisor n (+ test 1)))))
  (find-divisor n 2))

(define (prime? n)
  (if (< n 2)
      nil
      (= n (smallest-divisor n))))

(prime? 7)    ;=> t
(prime? 12)   ;=> nil
(prime? 1009) ;=> t

;;; ============================================================
;;; 1.3 Formulating Abstractions with Higher-Order Procedures
;;; ============================================================

;;; 1.3.1 Procedures as Arguments

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (identity x) x)

;;; Sum of integers
(define (sum-integers a b)
  (sum identity a inc b))

(sum-integers 1 10)  ;=> 55

;;; Sum of cubes
(define (cube x) (* x x x))

(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 1 10)  ;=> 3025

;;; Pi sum approximation (SICP 1.3.1)
;;; pi/8 = 1/(1*3) + 1/(5*7) + 1/(9*11) + ...
(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x) (+ x 4))
  (sum pi-term a pi-next b))

;;; Note: sum is tree-recursive, so limited to ~80 terms by depth limit.
;;; For larger ranges, use sum-iter defined below.
(* 8.0 (pi-sum 1 80))  ;=> ~3.12 (approximation of pi, 20 terms)

;;; Iterative sum (tail-recursive)
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(sum-iter cube 1 inc 10)  ;=> 3025

;;; Product (Exercise 1.31)
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

;;; Factorial via product
(define (factorial-product n)
  (product identity 1 inc n))

(factorial-product 6)  ;=> 720

;;; Accumulate (Exercise 1.32)
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (sum-acc term a next b)
  (accumulate + 0 term a next b))

(define (product-acc term a next b)
  (accumulate * 1 term a next b))

(sum-acc cube 1 inc 10)      ;=> 3025
(product-acc identity 1 inc 6) ;=> 720

;;; Filtered accumulate (Exercise 1.33)
(define (filtered-accumulate combiner null-value term a next b predicate)
  (define (iter a result)
    (if (> a b)
        result
        (if (predicate a)
            (iter (next a) (combiner result (term a)))
            (iter (next a) result))))
  (iter a null-value))

;;; Sum of squares of primes in range
(define (sum-sq-primes a b)
  (filtered-accumulate + 0 square a inc b prime?))

(sum-sq-primes 2 10)  ;=> 4+9+25+49 = 87

;;; 1.3.2 Constructing Procedures Using Lambda

(define (make-adder n)
  (lambda (x) (+ n x)))

(define add5 (make-adder 5))

(add5 10)  ;=> 15

;;; let as syntactic sugar
(let ((x 3)
      (y 4))
  (+ x y))  ;=> 7

;;; 1.3.3 Procedures as General Methods

;;; Fixed-point iteration (SICP 1.3.3)
(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;;; Golden ratio: fixed point of x -> 1 + 1/x
(fixed-point (lambda (x) (+ 1.0 (/ 1.0 x))) 1.0)  ;=> ~1.6180

;;; Square root via fixed point with average damping
(define (sqrt-fp x)
  (fixed-point (lambda (y) (/ (+ y (/ x y)) 2.0)) 1.0))

(sqrt-fp 2.0)  ;=> ~1.4142

;;; 1.3.4 Procedures as Returned Values

(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2.0)))

;;; Cube root via average damping
(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))

(cube-root 27.0)  ;=> ~3.0
(cube-root 8.0)   ;=> ~2.0

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeat f n)
  (if (= n 1)
      f
      (compose f (repeat f (- n 1)))))

((compose square inc) 6)    ;=> 49
((repeat square 2) 5)       ;=> 625
((repeat inc 10) 0)         ;=> 10

;;; Newton's method (SICP 1.3.4)
(define dx 0.00001)

(define (deriv-fn g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv-fn g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;;; Square root via Newton's method: find zero of y -> y^2 - x
(define (sqrt-newton x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))

(sqrt-newton 2.0)  ;=> ~1.4142
(sqrt-newton 9.0)  ;=> ~3.0
