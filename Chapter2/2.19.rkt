(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 50 25 10 5 1))

;; test cases
(cc 100 us-coins) ;; 292

(define us-coins (list 1 5 10 25 50))

(cc 100 us-coins) ;; 292

;; The order in the list does not affect the result.
;; The program counts how many ways if there to make the change.
;; It has to calculate over all numbers of the list and enumerate every possibility, hence the order of the coins
;; into the list does matter, because it counts all the possibilities of make change with every coin from the list.
