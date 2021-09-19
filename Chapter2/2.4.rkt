#lang sicp

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(equal? (car (cons 2 3)) 2)
(equal? (cdr (cons 2 3)) 3)
