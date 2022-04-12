(define (square x)
  (* x x))

(define (exptt b n)
  (expt-iter-fast 1 b n))

(define (expt-iter-fast a b n)
  (cond ((= n 0) a)
        ((even? n) (expt-iter-fast a (square b) (/ n 2)))
        (else (expt-iter-fast (* a b) b (- n 1)))))
