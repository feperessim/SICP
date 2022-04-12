#lang SICP

;; Iterative product procedure

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (* (term a) result))))
  (iter a 1))

(define (identity x) x)

(define (inc n) (+ n 1))

;; Recursive product procedure

(define (product-recur term a next b)
  (if (> a b)
        1
        (* (term a)
           (product-recur term (next a) next b))))  

;; Factorial Procedure

(define (factorial n)
  (define (fac a b)
    (product identity a inc b))
  (fac 1 n))

(define (square x)
  (* x x))

;; Pi aproximation

(define (pi-aproximation n)
  (define (term a)
    (cond ((even? a)
        (square a))
        (else (/ 1.0 (square a)))))
  (/ (* 2.0 (product term 3 inc n)) n))


(factorial 5)
(factorial 6)
(factorial 7)
(* 4 (pi-aproximation 10.0))
(* 4 (pi-aproximation 100.0))
(* 4 (pi-aproximation 1000.0))
(* 4 (pi-aproximation 10000.0))
(* 4 (pi-aproximation 1000000.0))

       

