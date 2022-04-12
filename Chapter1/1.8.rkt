(define (square x)
  ( * x x))

(define (cbrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (cbrt-iter (improve guess x) x)))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2.0 guess)) 3.0))

(define (good-enough? guess x)
  (< (abs-error guess x) 0.0000000001))

(define (abs-error guess x)
  (abs (/ (- guess (improve guess x)) guess)))

(define (cbrt x)
  (cbrt-iter 1 x))
