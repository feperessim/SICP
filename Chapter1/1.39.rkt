(define (square x)
  (* x x))

(define (tan-cf x k)
  (define (try i)
    (cond ((>= i k ) i)
          ((= i 0) (/ x
                      (- 1
                         (try (+ i 3)))))
          (else (/ (square x)
                   (- i
                      (try (+ i 2)))))))
  (try 0.0))

(tan-cf 45 10000)