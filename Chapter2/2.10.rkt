#lang racket

(define (make-interval a b) (cons a b))

(define (lower-bound z)
  (car z))

(define (upper-bound z)
  (cdr z))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (add-interval x
                (make-interval (- (upper-bound y))
                               (- (lower-bound y)))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
  (make-interval (min p1 p2 p3 p4)
                 (max p1 p2 p3 p4))))

(define (div-interval x y)
  (cond ((<= (* (lower-bound y)
                (upper-bound y)) 0)
      (display "The interval [")
      (display (lower-bound y))
      (display ", ")
      (display (upper-bound y))
      (display"], spans zero")
      (newline))
  (else (mul-interval x
                      (make-interval (/ 1.0 (upper-bound y))
                                     (/ 1.0 (lower-bound y)))))))

;; test cases
(div-interval (make-interval 1 2)
              (make-interval 3 4))

(div-interval (make-interval 1 2)
              (make-interval 3 0))

(div-interval (make-interval 1 2)
              (make-interval 0 4))

(div-interval (make-interval 0 2)
              (make-interval 3 4))

(div-interval (make-interval -2 2)
              (make-interval -3 4))

(div-interval (make-interval 0 2)
              (make-interval -3 -4))

