(define ( double a)
  (* 2 a))

(define (halve a)
  (/ a 2))

(define (iter-mult a b)
  (fast-iter-mult 0 a b))

(define (fast-iter-mult k a b)
  (cond ((= b 0) k)
        ((even? b) (fast-iter-mult k (double a) (halve b)))
        (else (fast-iter-mult (+ k a) a (- b 1)))))
