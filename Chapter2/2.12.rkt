#lang racket

(define (make-interval a b) (cons a b))

(define (lower-bound z)
  (car z))

(define (upper-bound z)
  (cdr z))

(define (make-center-percent c p)
  (let ((percentage-tolerance (* c (/ p 100.0))))
  (make-interval (- c percentage-tolerance) (+ c percentage-tolerance))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percentage-tolerance i)
    (* (/ (width i) (center i)) 100))
