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
  (let ((x-lower-bound (lower-bound x))
        (x-upper-bound (upper-bound x))
        (y-lower-bound (lower-bound y))
        (y-upper-bound (upper-bound y)))
    (cond ((and (>= x-lower-bound 0)
                (>= x-upper-bound 0)
                (>= y-lower-bound 0)
                (>= y-upper-bound 0))
           (make-interval (* x-lower-bound
                             y-lower-bound)
                          (* x-upper-bound
                             y-upper-bound)))

          ((and (>= x-lower-bound 0)
                (>= x-upper-bound 0)
                (< y-lower-bound 0)
                (>= y-upper-bound 0))
           (make-interval (* x-upper-bound
                             y-lower-bound)
                          (* x-upper-bound
                             y-upper-bound)))

          ((and (>= x-lower-bound 0)
                (>= x-upper-bound 0)
                (< y-lower-bound 0)
                (< y-upper-bound 0))
           (make-interval (* x-upper-bound
                             y-lower-bound)
                          (* x-lower-bound
                             y-upper-bound)))

          ((and (< x-lower-bound 0)
                (>= x-upper-bound 0)
                (>= y-lower-bound 0)
                (>= y-upper-bound 0))
           (make-interval (* x-lower-bound
                             y-upper-bound)
                          (* x-upper-bound
                             y-upper-bound)))

          ((and (< x-lower-bound 0)
                (>= x-upper-bound 0)
                (< y-lower-bound 0)
                (>= y-upper-bound 0))
           (make-interval (min (* x-upper-bound y-lower-bound)
                               (* x-lower-bound y-upper-bound))
                          (max (* x-lower-bound y-lower-bound)
                               (* x-upper-bound y-upper-bound))))
          
           ((and (< x-lower-bound 0)
                (>= x-upper-bound 0)
                (< y-lower-bound 0)
                (< y-upper-bound 0))
           (make-interval (* x-upper-bound
                             y-lower-bound)
                          (* x-lower-bound
                             y-lower-bound)))
           
            ((and (< x-lower-bound 0)
                (< x-upper-bound 0)
                (< y-lower-bound 0)
                (>= y-upper-bound 0))
           (make-interval (* x-lower-bound
                             y-upper-bound)
                          (* x-lower-bound
                             y-lower-bound)))
           
            ((and (< x-lower-bound 0)
                (< x-upper-bound 0)
                (>= y-lower-bound 0)
                (>= y-upper-bound 0))
           (make-interval (* x-lower-bound
                             y-upper-bound)
                          (* x-upper-bound
                             y-lower-bound)))
            
          ((and (< x-lower-bound 0)
                (< x-upper-bound 0)
                (< y-lower-bound 0)
                (< y-upper-bound 0))
           (make-interval (* x-upper-bound
                             y-upper-bound)
                          (* x-lower-bound
                             y-lower-bound))))))
;; test cases

(define a (make-interval 3 7))
(define b (make-interval -7 10))
(define c (make-interval -9 -2))
(mul-interval a a)
(mul-interval a b)
(mul-interval a c)
(mul-interval b a)
(mul-interval b b)
(mul-interval b c)
(mul-interval c a)
(mul-interval c b)
(mul-interval c c)
