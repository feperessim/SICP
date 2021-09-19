(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (square x)
  (* x x))

(define (repeated f x)
  (if (= x 1)
      f
      (repeated (compose f f) (- x 1))))

(define dx 0.00001)

(define (f-average f)
  (lambda (x)
         (/
          (+ (f (- x dx))
             (f x)
             (f (+ x dx)))
          3)))

(define (n-fold f x)
  ((repeated f-average x) f))

(define (square x)
  (* x x))
((n-fold square 2) 2)