(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m ))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fooling-fermat-test n)
  (define (fool-fermat a n)
    (cond ((= (expmod a n n) a)
           (fool-fermat (+ a 1) n))
          ((= a n) #t)
          (else #f)))
    (fool-fermat 2 n))

(define (square x)
  (* x x))

(fooling-fermat-test 561)
(fooling-fermat-test 1105)
(fooling-fermat-test 1729)
(fooling-fermat-test 2465)
(fooling-fermat-test 2821)
(fooling-fermat-test 6601)

;; All procedure's call are evaluated to true.
;; #t
;; #t
;; #t
;; #t
;; #t
;; #t