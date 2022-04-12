(define (square x) (* x x))

(define (sum-of-squares a b)
  (+ (square a) (square b)))

(define (smallest a b)
  (if (< a b) a b))

(define (smallest-of-three a b c)
  (smallest (smallest a b) c))

(define (sum-of-squares-of-two-largest a b c)
  (cond ((equal? a (smallest-of-three a b c)) (sum-of-squares b c))
        ((equal? b (smallest-of-three a b c)) (sum-of-squares a c))
        (else (sum-of-squares a b))))
      
