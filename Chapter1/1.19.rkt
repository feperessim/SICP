(define (fib-t n a b p q)
  (cond ((= n 0) b)
        (else (fib-t (- n 1) (+ (* b q) (* a q) (* a p))
                   (+ (* b p) (* a q)) p q))))

(define (fib-t-prime n a b p q)
  (cond ((= n 0) b)
        (else (fib-t-prime (- n 1)
                   (+ (* a (+ (square p) (square q)))
                   (* (+ a b) (+ (square q) (* 2 p q))))
                   (+ (* b (+ (square b) (square q)))
                   (* a (+ (square q) (* 2 p q))))
                   p q))))
      
(define (f n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (f (- n 1)) (f (- n 2))))))

;; a <- bq + aq + ap
;; b <- bp + aq
;;
;; a' <- q(bp + aq) + q(bq + aq + ap) + p (bq +aq + ap)
;;      pbq + aq^2 + bq^2 + aq^2 + apq + pbq + apq + ap^2
;;      ap^2 + aq^2 +aq^2 + apq + apq + pbq + pbq + pbq  + bq^2
;;      a(p^2 + 2q^2 + 2pq) + b(2pq + q^2)
;;      a(p2^+ q^2) + a(q^2 +2pq) + b(2pq + q^2)
;;      a(p2^+ q^2) + (a + b)(2pq + q^2)
;;
;;
;; b' <- p(bp +aq) + q(bq + aq + ap)
;;      p^2b +  apq + bq^2 + aq^2 + apq
;;      p^2b + bq^2 + aq^2 + 2apq
;;      b(p^2 + q^2) + a(q^2 + 2pq)


;; Thus when a = 1, b = 0 it implies in


;; 1(p2^+ q^2) + (1 + 0)(pq + q^2) = >  (p2^+ q^2) + (pq + q^2)

;; 0(p^q + q^2) +1(q^2 + 2pq) = > (q^2 + 2pq)

;; then

;; p' = (p^2+ q^2)
;; q' = (q^2 + 2pq)

(define (square x)
  (* x x))

(define (fib n)
  (fib-iter 1 0 0 1 n))

  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (square p) (square q))
                     (+ (square q) (* 2 p q))
                     (/ count 2)))
           (else (fib-iter (+ (* b q) (* a q) (* a p))
                           (+ (* b p) (* a q))
                           p
                           q
                           (- count 1)))))
