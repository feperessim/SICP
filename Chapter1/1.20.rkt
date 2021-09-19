#lang sicp

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; Substitution method for normal-order evaluation

(gcd 206 40)
(if (= 40 0)
      206
      (gcd 40 (remainder 206 40))))
40 6
(gcd 40 (remainder 206 40))
(if (= 6 0)
      (gcd (remainder 206 40)
           (remainder 40 (remainder 206 40))))
6 4
(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
(if (= 4 0)
      (remainder 206 40)
      (gcd (remainder 40 (remainder 206 40))
           (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
4 2
(gcd (remainder 40 (remainder 206 40))
     (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
(if (= 2 0))
  (remainder 40 (remainder 206 40))
(gcd  (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
      (remainder (remainder 40 (remainder 206 40))
                 (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
2 0
(gcd  (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
      (remainder (remainder 40 (remainder 206 40))
                 (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
(if (= (remainder (remainder 40 (remainder 206 40))
                 (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))) 0 0)
    (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))

(if (= 0 0)
    2)

;; Eighteen Remainder Operations are performed in the normal order of evaluation.

;; ==============================================================================

;; Substitution method for applicative-order evaluation

(gcd 206 40)
(if (= 40 0)
    206
    (gcd 40 6))

(gcd 40 6)
(if (= 6 0)
    40
    (gcd 6 4))

(gcd 6 4)
(if (= 4 0)
    6
    (gcd 4 2))

(gcd 4 2)
(if (= 2 0)
    4
    (gcd 2 0))

(gcd 2 0)
2

;; Four Remainder Operations are performed in the applicative-order of evaluation, due this order follows the substitution model for Procedure application
;; whereas it firstly evaluate the formal parameters of all subexpressions and then aplies the procedures.