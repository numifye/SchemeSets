;Author:    Naomi Campbell

#lang r5rs
(#%require (only scheme/base error)) ;note: added this, for some reason got rid of error I was getting

(define (empty? s)
  (if(null? s)#t #f)
)

(define (set s)
  (cond((null? s) '())
       ((in? (car s)(cdr s)) (set(cdr s)))
       (else (cons (car s) (set (cdr s)))))
)

(define (in? e s)
  (cond((null? s) #f)
       ((equal? e(car s)) #t)
       (else (in? e (cdr s))))
)

(define (add e s)
  ;if already in s, return s; else construct e + s --> call set to remove repeats
  (if (in? e s) s (set (cons e s)))
)

(define (discard e s)
  (cond ((null? s) '())
        ((equal? e (car s)) (cdr s))
        (else (cons (car s) (discard e (cdr s)))))
)

(define (union s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        ((in? (car s1) s2)(union(cdr s1)s2))
        (else(cons(car s1)(union(cdr s1) s2))))
)

(define (intersection s1 s2)
  (cond ((or (null? s1) (null? s2)) '())
        ((in? (car s1) s2) (cons (car s1) (intersection (cdr s1) s2)))
        (else (intersection (cdr s1) s2)))
)

(define (difference s1 s2) ;double check code
  (cond ((or (null? s1) (null? s2)) '())
        ((in? (car s1) s2) (difference (cdr s1) s2))
        (else (cons (car s1) (difference (cdr s1) s2))))
)

(define (symmetric-difference s1 s2)
  (cond ((null? (intersection s1 s2)) (union s1 s2)) ;if intersection is empty then maybe 1 list or both lists are empty... return the union (whatever's left)
        ;return intersection of (union list w/ 1st element in intersection removed) & (sym-diff of both lists with 1st element in intersection removed))
        ;use intersection to make list instead of cons --> no repeats; the final function call will intersect the FINAL symmetric-difference list, which will return that list
        ;this took a while
        (else (intersection (discard (car (intersection s1 s2)) (union s1 s2)) (symmetric-difference (discard (car (intersection s1 s2)) s1) (discard (car (intersection s1 s2)) s2)))))
)

(define (subset? s1 s2)
  ;every element in s1 is in s2
  ;the empty set is a subset of every set
  (cond ((null? s1) #t) ;use 'empty?' instead?
        (else (and (in? (car s1) s2) (subset? (cdr s1) s2))))
)
          
(define (superset? s1 s2)
    ;every element in s2 is in s1
  (cond ((null? s2) #t) ;use 'empty?' instead?
        (else (and (in? (car s2) s1) (subset? (cdr s2) s1))))
)

(define (disjoint? s1 s2)
  (cond (null? (intersection s1 s2) #t)
        (else #f))
)

(define (sameset? s1 s2)
  ;if A is a subset of B and B is a subset of A
  (cond ((and (subset? s1 s2) (subset? s2 s1)) #t)
        (else #f))
)


; some tests
(define A (set '(1 2 7 9 7 1)))
(define B (set '(2 0 8 0 7 12)))
(define C (set '(9 7)))

(define colors (set '("yellow" "red" "green" "blue" "orange" "purple" "pink")))
(define rgb (set '("red" "green" "blue")))

(define hi (set '(#\h #\i)))

(empty? A) ; #f
(empty? rgb) ;#f
(empty? (set'())) ;#t

(in? 0 A) ; #f
(in? "red" A); #f
(in? 2 A) ; #t

(in? "green" rgb) ; #t
(in? "purple" rgb) ; #f
(in? "i" hi) ;#f
(in? #\i hi) ;#t

(add 9 A) ; (2 9 7 1)
(add 5 A) ; (5 2 9 7 1)

(discard 1 A) ; (2 9 7)
(discard 5 A) ; (2 9 7 1)
(union A B) ; (9 1 2 8 0 7 12)
(union A rgb) ; (2 9 7 1 "red" "green" "blue")

(intersection A rgb) ; ()
(intersection A B) ; (2 7)
(intersection rgb colors) ; ("red" "green" "blue")

(difference A B) ; (9 1)
(difference rgb colors) ; ()
(difference colors rgb) ; ("yellow" "orange" "purple" "pink")

(symmetric-difference A B) ; (9 1 8 0 12)
(symmetric-difference A C) ; (2 1)
(symmetric-difference colors rgb) ; ("yellow" "orange" "purple" "pink")

(subset? A B) ;#f
(subset? C A) ; #t

(subset? colors rgb) ;#f
(subset? rgb colors)  ; #t

(superset? A B) ;#f
(superset?  A C) ; #t

(superset? colors rgb) ;#t
(superset? rgb colors)  ; #f

(disjoint? B C) ;#f
(disjoint? colors A) ;#t

(sameset? (set '(9 1 2 7)) A); #t
(sameset? B A) ; #f