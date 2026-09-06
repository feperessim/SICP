(define (make-accumulator value)
  (lambda (amount)
    (set! value (+ amount value))
    value))

(define A (make-accumulator 0))
(define B (make-accumulator 10))

(A 1) ;; => 1
(A 2) ;; => 3
(A 3) ;; => 6
(A 4) ;; => 10
(A 5) ;; => 15
(B 10) ;; => 20
