#lang SICP

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (square x)
  (* x x))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (cond ((prime? n)
      (report-prime (- (runtime) start-time))
      #t)
      (else #f)))
      

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes range count-primes-found)
  (cond ((= count-primes-found 3) (newline))
        ((even? range)
        (search-for-primes (+ range 1)
                           count-primes-found))
        ((timed-prime-test range)
         (search-for-primes (+ range 2)
                           (+ count-primes-found 1)))
        (else
         (search-for-primes (+ range 2)
                           count-primes-found))))
 
;; Results
;; > (search-for-primes 1000 0)

;; 1009 *** 0
;; 1013 *** 0
;; 1019 *** 0
;; > (search-for-primes 10000 0)

;; 10007 *** 0
;; 10009 *** 0
;; 10037 *** 0
;;> (search-for-primes 100000 0)

;; 100003 *** 0
;; 100019 *** 0
;; 100043 *** 500
;; > (search-for-primes 1000000 0)

;; 1000003 *** 500
;; 1000033 *** 1000
;; 1000037 *** 999
;;>

;; The data for time at 1000 and 10000 doesn't show any difference at all.
;; The data for 100,000 and for 1,000,00 supports very well the prediction,
;; the time taken to find those primes are <= to the half of their square root.
;; To primes near 1,000,000 the prediction is even more accuratte.

;; The results seems to be compatible when the input becomes large, otherwise
;; it seems to not make any difference at all as far as computers nowadays are
;; too fast.

;; (search-for-primes 100000000 0)
;; 100000007 *** 6499
;; 100000037 *** 6501
;; 100000039 *** 6501

;; For 1E8 the time is less than the sqrt n, it is .64, .65 times the sqrt(n).
;; Given this fact, then there is a constant dividing the sqrt(n), where in
;; order of growth does not makes any difference, so the order of growth is still sqrt(n)


;; (search-for-primes 10000000000 0)
;; 10000000019 *** 87000
;; 10000000033 *** 82499
;; 10000000061 *** 81000

;; For 1E8 the time is less than the sqrt n, it is .87, .87 times the sqrt(n).
;; Given this fact, then there is a constant dividing the sqrt(n), where in
;; order of growth does not makes any difference, so the order of growth is still sqrt(n)



