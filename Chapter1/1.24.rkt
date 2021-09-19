(#%require (only racket/base current-milliseconds))
(define (runtime) (current-milliseconds))
 (#%require racket/random)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m ))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
(try-it (+ 1 (random-ref (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (prime? n) 
   (fast-prime? n 10))

(define (square x)
  (* x x))

(timed-prime-test 1009)
(timed-prime-test 1013)
(timed-prime-test 1019)

(timed-prime-test 10007)
(timed-prime-test 10009)
(timed-prime-test 10037)

(timed-prime-test 100003)
(timed-prime-test 100019)
(timed-prime-test 100043)

(timed-prime-test 1000003) 
(timed-prime-test 1000033)
(timed-prime-test 1000037)

;; results for 10 tries

;; 1009 *** 0
;; 1013 *** 0
;; 1019 *** 15
;; 10007 *** 47
;; 10009 *** 31
;; 10037 *** 46
;; 100003 *** 609
;; 100019 *** 425
;; 100043 *** 484
;; 1000003 *** 3818
;; 1000033 *** 3725
;; 1000037 *** 3725

;; results for 100 tries

;; 1009 *** 47
;; 1013 *** 46
;; 1019 *** 32
;; 10007 *** 391
;; 10009 *** 360
;; 10037 *** 388
;; 100003 *** 4089
;; 100019 *** 3884
;; 100043 *** 4075
;; 1000003 *** 39945
;; 1000033 *** 37619
;; 1000037 *** 38121

;; The discrepancy occurs as far as the number of tries get augmented, otherwise te test get too much longer
