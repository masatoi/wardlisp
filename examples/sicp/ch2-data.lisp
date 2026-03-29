;;; SICP Chapter 2: Building Abstractions with Data
;;; Adapted for wardlisp (no mutation, no built-in map/filter)

;;; ============================================================
;;; 2.1 Introduction to Data Abstraction
;;; ============================================================

;;; 2.1.1 Rational Number Arithmetic
;;; Represent rationals as pairs (numer . denom)

(define (gcd a b)
  (if (= b 0) a (gcd b (mod a b))))

(define (abs x) (if (< x 0) (- x) x))

(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d))))
    (if (< d 0)
        (cons (- (/ n g)) (/ (- d) g))
        (cons (/ n g) (/ d g)))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))

(add-rat one-half one-third)          ;=> (5 . 6)
(mul-rat one-half one-third)          ;=> (1 . 6)
(add-rat one-third one-third)         ;=> (2 . 3)
(equal-rat? (make-rat 2 4) one-half)  ;=> t

;;; 2.1.3 What Is Meant by Data?
;;; Procedural representation of pairs

(define (my-cons x y)
  (lambda (m) (if (= m 0) x y)))

(define (my-car z) (z 0))
(define (my-cdr z) (z 1))

(my-car (my-cons 3 4))  ;=> 3
(my-cdr (my-cons 3 4))  ;=> 4

;;; Church numerals (Exercise 2.6)
(define (church-zero f) (lambda (x) x))

(define (church-add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define church-one
  (lambda (f) (lambda (x) (f x))))

(define church-two
  (lambda (f) (lambda (x) (f (f x)))))

(define (church-add m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))

;;; Convert Church numeral to integer
(define (church-to-int n)
  ((n (lambda (x) (+ x 1))) 0))

(church-to-int church-zero)                    ;=> 0
(church-to-int church-one)                     ;=> 1
(church-to-int church-two)                     ;=> 2
(church-to-int (church-add church-two church-two)) ;=> 4

;;; ============================================================
;;; 2.2 Hierarchical Data and the Closure Property
;;; ============================================================

;;; 2.2.1 Representing Sequences

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))
(list-ref squares 3)  ;=> 16

(define (my-length items)
  (define (iter a count)
    (if (null? a)
        count
        (iter (cdr a) (+ count 1))))
  (iter items 0))

(my-length squares)  ;=> 5

(define (my-append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (my-append (cdr list1) list2))))

(my-append (list 1 2 3) (list 4 5 6))  ;=> (1 2 3 4 5 6)

;;; Mapping over lists
(define (my-map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (my-map proc (cdr items)))))

(my-map square (list 1 2 3 4 5))  ;=> (1 4 9 16 25)
(my-map abs (list -1 2 -3 4 -5))  ;=> (1 2 3 4 5)

;;; Filter
(define (my-filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (my-filter predicate (cdr sequence))))
        (t (my-filter predicate (cdr sequence)))))

(define (odd? n) (= (mod n 2) 1))
(define (even? n) (= (mod n 2) 0))

(my-filter odd? (list 1 2 3 4 5 6 7))  ;=> (1 3 5 7)

;;; Reduce / accumulate
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5))        ;=> 15
(accumulate * 1 (list 1 2 3 4 5))        ;=> 120
(accumulate cons nil (list 1 2 3 4 5))   ;=> (1 2 3 4 5)

;;; Enumerate an interval
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 2 7)  ;=> (2 3 4 5 6 7)

;;; Flatmap
(define (flatmap proc seq)
  (accumulate append nil (my-map proc seq)))

;;; 2.2.2 Hierarchical Structures (Trees)

(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((atom? tree) 1)
        (t (+ (count-leaves (car tree))
              (count-leaves (cdr tree))))))

(define tree1 (cons (list 1 2) (list 3 4)))
(count-leaves tree1)  ;=> 4

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((atom? tree) (* tree factor))
        (t (cons (scale-tree (car tree) factor)
                 (scale-tree (cdr tree) factor)))))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)
;=> (10 (20 (30 40) 50) (60 70))

(define (tree-map proc tree)
  (cond ((null? tree) nil)
        ((atom? tree) (proc tree))
        (t (cons (tree-map proc (car tree))
                 (tree-map proc (cdr tree))))))

(tree-map square (list 1 (list 2 3) (list 4 (list 5))))
;=> (1 (4 9) (16 (25)))

;;; 2.2.3 Sequences as Conventional Interfaces

;;; Sum of squares of odd elements in a tree
(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((atom? tree)
         (if (odd? tree) (square tree) 0))
        (t (+ (sum-odd-squares (car tree))
              (sum-odd-squares (cdr tree))))))

(sum-odd-squares (list 1 2 (list 3 4) 5))  ;=> 1+9+25 = 35

;;; Even fibs as list
(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0) b (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        nil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

(even-fibs 10)  ;=> (0 2 8 34)

;;; Nested mappings: generate pairs (i,j) where 1<=j<i<=n
(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (my-map (lambda (j) (list i j))
             (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(unique-pairs 4)  ;=> ((2 1) (3 1) (3 2) (4 1) (4 2) (4 3))

;;; Prime-sum pairs
(define (prime? n)
  (define (smallest-divisor n)
    (define (find-divisor test)
      (cond ((> (* test test) n) n)
            ((= (mod n test) 0) test)
            (t (find-divisor (+ test 1)))))
    (find-divisor 2))
  (if (< n 2) nil (= n (smallest-divisor n))))

(define (prime-sum-pairs n)
  (my-filter
   (lambda (pair)
     (prime? (+ (car pair) (car (cdr pair)))))
   (unique-pairs n)))

(prime-sum-pairs 6)
;=> pairs (i,j) where i+j is prime

;;; ============================================================
;;; 2.3 Symbolic Data
;;; ============================================================

;;; 2.3.1 Quotation

(define (memq item x)
  (cond ((null? x) nil)
        ((eq? item (car x)) x)
        (t (memq item (cdr x)))))

(memq 'apple '(pear banana prune))         ;=> nil
(memq 'apple '(x (apple sauce) y apple pear)) ;=> (apple pear)

;;; 2.3.2 Symbolic Differentiation

;;; Representation
(define (variable? x)
  (and (atom? x) (not (number? x))))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((and (number? a1) (= a1 0)) a2)
        ((and (number? a2) (= a2 0)) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (t (list 'plus a1 a2))))

(define (make-product m1 m2)
  (cond ((and (number? m1) (= m1 0)) 0)
        ((and (number? m2) (= m2 0)) 0)
        ((and (number? m1) (= m1 1)) m2)
        ((and (number? m2) (= m2 1)) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (t (list 'times m1 m2))))

(define (sum? x)
  (and (not (atom? x)) (eq? (car x) 'plus)))

(define (addend s) (car (cdr s)))
(define (augend s) (car (cdr (cdr s))))

(define (product? x)
  (and (not (atom? x)) (eq? (car x) 'times)))

(define (multiplier p) (car (cdr p)))
(define (multiplicand p) (car (cdr (cdr p))))

;;; Differentiation
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (t nil)))

;;; d/dx (x + 3) = 1
(deriv '(plus x 3) 'x)  ;=> 1

;;; d/dx (x * y) = y
(deriv '(times x y) 'x)  ;=> y

;;; d/dx (x * y + x * 3) = (plus y 3)
(deriv '(plus (times x y) (times x 3)) 'x)

;;; ============================================================
;;; 2.3.3 Representing Sets
;;; ============================================================

;;; Sets as unordered lists

(define (element-of-set? x set)
  (cond ((null? set) nil)
        ((equal? x (car set)) t)
        (t (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) nil)
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (t (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (t (cons (car set1) (union-set (cdr set1) set2)))))

(element-of-set? 3 '(1 2 3 4))  ;=> t
(adjoin-set 5 '(1 2 3))         ;=> (5 1 2 3)
(intersection-set '(1 2 3) '(2 3 4))  ;=> (2 3)
(union-set '(1 2 3) '(2 3 4))         ;=> (1 2 3 4)

;;; Sets as ordered lists (integers)

(define (element-of-ordered? x set)
  (cond ((null? set) nil)
        ((= x (car set)) t)
        ((< x (car set)) nil)
        (t (element-of-ordered? x (cdr set)))))

(define (intersection-ordered set1 set2)
  (cond ((or (null? set1) (null? set2)) nil)
        ((= (car set1) (car set2))
         (cons (car set1)
               (intersection-ordered (cdr set1) (cdr set2))))
        ((< (car set1) (car set2))
         (intersection-ordered (cdr set1) set2))
        (t (intersection-ordered set1 (cdr set2)))))

(intersection-ordered '(1 2 3 5 7) '(2 3 4 5 8))  ;=> (2 3 5)

;;; Sets as binary search trees
;;; Tree = nil | (entry left right)

(define (entry tree) (car tree))
(define (left-branch tree) (car (cdr tree)))
(define (right-branch tree) (car (cdr (cdr tree))))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-tree? x tree)
  (cond ((null? tree) nil)
        ((= x (entry tree)) t)
        ((< x (entry tree))
         (element-of-tree? x (left-branch tree)))
        (t (element-of-tree? x (right-branch tree)))))

(define (adjoin-tree x tree)
  (cond ((null? tree) (make-tree x nil nil))
        ((= x (entry tree)) tree)
        ((< x (entry tree))
         (make-tree (entry tree)
                    (adjoin-tree x (left-branch tree))
                    (right-branch tree)))
        (t (make-tree (entry tree)
                      (left-branch tree)
                      (adjoin-tree x (right-branch tree))))))

(define (tree-to-list tree)
  (define (copy-to-list tree result)
    (if (null? tree)
        result
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result)))))
  (copy-to-list tree nil))

(define sample-tree
  (adjoin-tree 7
    (adjoin-tree 3
      (adjoin-tree 9
        (adjoin-tree 1
          (adjoin-tree 5 (make-tree 6 nil nil)))))))

(tree-to-list sample-tree)  ;=> sorted list
(element-of-tree? 5 sample-tree)  ;=> t
(element-of-tree? 4 sample-tree)  ;=> nil

;;; ============================================================
;;; 2.3.4 Huffman Encoding Trees
;;; ============================================================

;;; Leaf representation
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (and (not (atom? object)) (eq? (car object) 'leaf)))

(define (symbol-leaf x) (car (cdr x)))
(define (weight-leaf x) (car (cdr (cdr x))))

;;; Tree representation
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch-h tree) (car tree))
(define (right-branch-h tree) (car (cdr tree)))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (car (cdr (cdr tree)))))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (car (cdr (cdr (cdr tree))))))

;;; Decoding
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        nil
        (let ((next-branch
               (if (= (car bits) 0)
                   (left-branch-h current-branch)
                   (right-branch-h current-branch))))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

;;; Encoding
(define (encode-symbol symbol tree)
  (cond ((leaf? tree) nil)
        ((memq symbol (symbols (left-branch-h tree)))
         (cons 0 (encode-symbol symbol (left-branch-h tree))))
        ((memq symbol (symbols (right-branch-h tree)))
         (cons 1 (encode-symbol symbol (right-branch-h tree))))
        (t nil)))

(define (encode message tree)
  (if (null? message)
      nil
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;;; Build a sample tree: A(4), B(2), C(1), D(1)
(define sample-huff-tree
  (make-code-tree
   (make-leaf 'a 4)
   (make-code-tree
    (make-leaf 'b 2)
    (make-code-tree (make-leaf 'd 1)
                    (make-leaf 'c 1)))))

(decode '(0 1 1 0 0 1 0 1 0) sample-huff-tree)
;=> (a d a b b)

(encode '(a d a b b) sample-huff-tree)
;=> (0 1 1 0 0 1 0 1 0)

;;; ============================================================
;;; 2.4 Multiple Representations for Abstract Data
;;; ============================================================

;;; Tagged data: type tags using symbols
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (not (atom? datum))
      (car datum)
      nil))

(define (contents datum)
  (if (not (atom? datum))
      (cdr datum)
      nil))

;;; Rectangular representation of complex numbers (integer pairs)
(define (make-from-real-imag-rect x y)
  (attach-tag 'rectangular (cons x y)))

(define (real-part-rect z) (car (contents z)))
(define (imag-part-rect z) (cdr (contents z)))

;;; Polar representation (angle in milliradians, magnitude)
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

(define (magnitude-polar z) (car (contents z)))
(define (angle-polar z) (cdr (contents z)))

;;; Generic dispatch
(define (real-part z)
  (cond ((eq? (type-tag z) 'rectangular) (real-part-rect z))
        ((eq? (type-tag z) 'polar) (magnitude-polar z))
        (t nil)))

(define (imag-part z)
  (cond ((eq? (type-tag z) 'rectangular) (imag-part-rect z))
        ((eq? (type-tag z) 'polar) (angle-polar z))
        (t nil)))

(define z1 (make-from-real-imag-rect 3 4))
(real-part z1)  ;=> 3
(imag-part z1)  ;=> 4

;;; Add complex (rectangular result)
(define (add-complex z1 z2)
  (make-from-real-imag-rect
   (+ (real-part z1) (real-part z2))
   (+ (imag-part z1) (imag-part z2))))

(add-complex (make-from-real-imag-rect 1 2)
             (make-from-real-imag-rect 3 4))
;=> (rectangular 4 . 6)

;;; ============================================================
;;; 2.5 Systems with Generic Operations
;;; ============================================================

;;; Simple dispatch table using association lists (no mutation needed)

(define (assoc key table)
  (cond ((null? table) nil)
        ((eq? key (car (car table)))
         (car table))
        (t (assoc key (cdr table)))))

;;; Generic arithmetic: tag-based dispatch for integers and rationals

(define (make-integer n) (attach-tag 'integer n))
(define (make-rational n d) (attach-tag 'rational (make-rat n d)))

(define (generic-add x y)
  (let ((tx (type-tag x))
        (ty (type-tag y)))
    (cond ((and (eq? tx 'integer) (eq? ty 'integer))
           (make-integer (+ (contents x) (contents y))))
          ((and (eq? tx 'rational) (eq? ty 'rational))
           (make-rational
            (numer (add-rat (contents x) (contents y)))
            (denom (add-rat (contents x) (contents y)))))
          (t nil))))

(generic-add (make-integer 3) (make-integer 4))        ;=> (integer . 7)
(generic-add (make-rational 1 2) (make-rational 1 3))  ;=> (rational 5 . 6)
