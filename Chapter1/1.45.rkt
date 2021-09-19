(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define tolerance 0.0001)
(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f x)

  (if (= x 1)
      f
     (repeated  (compose f f) (- x 1))))

(define (root n x)
  (fixed-point ((repeated average-damp
                           (floor (/ (log n) (log 2))))
                           (lambda (y) (/ x (expt y (- n 1))))) 1.0))