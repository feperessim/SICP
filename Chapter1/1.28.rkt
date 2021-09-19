 (#%require racket/random)

(define (miller-rabin-test n)
   (define (try-it a)
    (= (expmod a (- n 1) n ) 1))
(try-it (+ 1 (random-ref (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square
                     (non-trivial-sqrt?
                      (expmod base (/ exp 2) m ) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

 (define (non-trivial-sqrt? n m)
   (if (and (not (or (= n 1)
                        (= n (- m 1 ))))
        (= (remainder (square n)  m) 1)) 
       0
       n))
   
(define (square x)
  (* x x))

;; Testing Miller-Rabbin on Previous primes found
(miller-rabin-test 1009)
(miller-rabin-test 1013)
(miller-rabin-test 1019)
(miller-rabin-test 10007)
(miller-rabin-test 10009)
(miller-rabin-test 10037)

(miller-rabin-test 100003)
(miller-rabin-test 100019)
(miller-rabin-test 100043)

(miller-rabin-test 1000003) 
(miller-rabin-test 1000033)
(miller-rabin-test 1000037)

;; Testing Miller-Rabbin on Carmichael numbers
(miller-rabin-test 561)
(miller-rabin-test 1105)
(miller-rabin-test 1729)
(miller-rabin-test 2465)
(miller-rabin-test 2821)
(miller-rabin-test 6601)

;;  Testing Miller-Rabbin on numbers from n to 2
(define (testing-miller-rabin n)
  (cond ((= n 1) (display "Done"))
        ((miller-rabin-test n) (display n) (newline)
                               (testing-miller-rabin (- n 1)))
        (else (testing-miller-rabin (- n 1)))))

(testing-miller-rabin 1000)