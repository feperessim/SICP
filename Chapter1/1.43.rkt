(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (square x)
  (* x x))

(define (repeated f x)
  (if (= x 1)
      f
      (repeated (compose f f) (- x 1))))

((repeated square 2) 5)