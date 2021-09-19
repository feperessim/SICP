#lang racket

;; Width of [a, b] is (b - a)/2 as well the width of [c, d] is (d - c)/a
;; The sum of two intervals [a, b] + [c, d] is [a+c, b+d]
;; The width of the sum is ((b+d) - (a+c))/2
;; Which by the commutative law of the sum can be rearranged to (b-a + d-c)/2
;; and thus break the into two rational numbers (b-a)/2 + (d-c)/2
;; which is the sum of the width of each interval.

;; The multiplication of two intervals [a,b], [c,d] are [min(a*c, a*d, b*c, b*d),  max(a*c, a*d, b*c, b*d)]
;; The width of the multiplication is (max(a*c, a*d, b*c, b*d) - min(a*c, a*d, b*c, b*d))/2
;; whose is not equal to the sum of the width of the intervals, look the intervals [2, 3], [-3, 4] for instance.
;; 

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


(define (width x)
  (/ 2.0 (- (upper-bound x)
            (lower-bound x))))

(equal? (+ (width (make-interval 2 3))
           (width (make-interval -3 4)))
        (width (mul-interval (make-interval 2 3)
                              (make-interval -3 4))))

(display (+ (width (make-interval 2 3))
            (width (make-interval -3 4))))
(newline)

(display (width (mul-interval (make-interval 2 3)
                               (make-interval -3 4))))
(newline)

        
                  