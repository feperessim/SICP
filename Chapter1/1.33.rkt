(define (filtered-accumulate filter? combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((filter? a)
         (combiner (term a)
                (filtered-accumulate
                 filter?
                 combiner
                 null-value
                 term (next a) next b)))
        (else (filtered-accumulate
               filter?
               combiner
               null-value
               term (next a) next b))))

(define (square x)
  (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (inc n) (+ n 1))

(define (term n)
  (square n))

(define sum +)

(define null-value-sum 0)

(define (sum-square-prime-numbers a b)
  (filtered-accumulate prime? sum null-value-sum term a inc b))

(define mult *)

(define null-value-mult 1)

(define (identity x) x)

(define (relatively-prime? i n)
  (= (gcd i n) 1))

(define (product-relative-prime a b)
  (define (filter? x)
    (relatively-prime? x b))
  (filtered-accumulate filter? mult null-value-mult identity a inc b))

;; test cases 
(sum-square-prime-numbers 2 3)  ;; 13
(sum-square-prime-numbers 2 5)  ;; 38
(sum-square-prime-numbers 2 9)  ;; 87
(sum-square-prime-numbers 2 11) ;; 208

;; test cases 
(product-relative-prime 2 3)  ;; 2
(product-relative-prime 2 5)  ;; 24
(product-relative-prime 2 9)  ;; 2240
(product-relative-prime 2 11) ;; 3628800
