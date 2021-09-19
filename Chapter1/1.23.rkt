(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x)
  (* x x))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (prime? n)
  (= n (smallest-divisor n)))

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


;; To numbers less than 1,000,000 there are no differences at all.

;; Old algorithm 
;; (search-for-primes 100000000 0)
;; 100000007 *** 6499
;; 100000037 *** 6501
;; 100000039 *** 6501


;; New one
;;> (timed-prime-test 100000007)
;; 100000007 *** 4998

;; > (timed-prime-test 100000037)
;; 100000037 *** 4000

;; > (timed-prime-test 100000039)
;; 100000039 *** 4499

;; Ratio old over new

;; > (/ 6499.0 4998)
;; 1.3003201280512204

;; > (/ 6501.0  4000)
;; 1.62525

;; > (/ 6501.0  4499)
;; 1.4449877750611246

;; ====================

;; Old algorithm

;; (search-for-primes 10000000000 0)
;; 10000000019 *** 87000
;; 10000000033 *** 82499
;; 10000000061 *** 81000

;; New one

;; > (/ 87000 68497.0)
;; 1.270128618771625

;; > (/ 82499 71500.0)
;; 1.153832167832168
;;

;; > (/ 81000 76499.0)
;; 1.0588373704231429

;; it is different from two because it is not only halving the problem, but eliminating every factor of two,
;; also halving the square root of the number given to be tested.
;; So it is equivalent to compute  the square root of two;
