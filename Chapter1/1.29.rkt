#lang SICP

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson-rule f a b n)
  (define h (/ (- b a) n))
  (simpson f a b n h 0))

(define (simpson f a b n h k)
  (define (add-a-kh k)
    (+ a (* k h)))
  (define (term k)
    (cond ((or (= k a)
               (>= k n))
           (f (add-a-kh k)))
          ((even? k)
           (* 2 (f (add-a-kh k))))
          (else
           (* 4 (f (add-a-kh k))))))
  (define (next k)
    (+ k 1))
  (* (/ h 3)
     (sum term k next n)))
   
(define (cube x)
  (* x (square x)))
(define (square x)
  (* x x))

(simpson-rule cube 0 1 100)
(simpson-rule cube 0 1 1000)
