(define (iterative-improve good-enough? improve-guess)
  (lambda (guess)
    (if (good-enough? guess)
      guess
      ((iterative-improve good-enough? improve-guess) (improve-guess guess)))))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- ((lambda (x) (* x x)) guess) x)) 0.001))
  (define (improve-guess guess)
    ((lambda (x y) (/ (+ x y) 2))  guess (/ x guess)))
  ((iterative-improve good-enough? improve-guess) 1.0))
    
(sqrt 625)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  (define (improve-guess guess)
    (f guess))
  ((iterative-improve close-enough? improve-guess) first-guess))

(fixed-point cos 1.0)
    


  