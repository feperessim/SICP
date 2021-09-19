#lang racket

(define (make-interval a b) (cons a b))

(define (lower-bound z)
  (car z))

(define (upper-bound z)
  (cdr z))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

;; To subtract two intervals we add the additive inverse
;; of the upper-bound of the second with the lower of the first

(define (sub-interval x y)
  (add-interval x
                (make-interval (- (upper-bound y))
                               (- (lower-bound y)))))