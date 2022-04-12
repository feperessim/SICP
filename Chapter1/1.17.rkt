(define ( double a)
  (* 2 a))

(define (halve a)
  (/ a 2))

(define (mult a b)
  (cond ((= b 0) b)
        ((even? b) (double (mult a (halve b))))
        (else (+ a ( mult a (- b 1)))))) 
