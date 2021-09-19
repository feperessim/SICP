(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1)
              (/ (n i)
             (+ result
                (d i))))))
  (iter k 0))

(newline)

(+ 2 (cont-frac-iter (lambda (i) 1.0)
                (lambda (i)
                  (cond ((= (remainder (+ i 1) 3) 0)
                         (* 2 (/ (+ i 1)  3)))
                        (else 1))) 100))                        

(define (cont-frac n d k)
  (define (try i)
    (if (= k i)
        i
        (/ (n i)
             (+ (d  i)
                (try (inc i))))))
  (try 1))

(+ 2 (cont-frac (lambda (i) 1.0)
                (lambda (i)
                  (cond ((= (remainder (+ i 1) 3) 0)
                         (* 2 (/ (+ i 1)  3)))
                        (else 1))) 100))

(+ 2 (cont-frac-did (lambda (i) 1.0)
                (lambda (i)
                  (cond ((= (remainder (+ i 1) 3) 0)
                         (* 2 (/ (+ i 1)  3)))
                        (else 1))) 100))