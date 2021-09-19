(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define phi (/ (+ 1 (sqrt 5)) 2))

(define (average x y)
  (/ (+ x y) 2))
  

;; Without average dumping 36 steps
(fixed-point (lambda (x) (/ (log 1000) (log x))) 1.5)
(newline)
;; With average dumping 12 steps
(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 1.5)
