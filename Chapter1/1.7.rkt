;; To very small values it may fails due the 'epsilon' choosen 0.001 not be enough to handle it.
;; Yet a very small value may be truncated to 0 when summed to the guess, thus failing to find the
;; correct square root.
;; With the below examples we can see how it fails to find the square root of values less than or equal to 0.0001

;; > (sqrt-iter 1 0.1)
;; 0.3162455622803890097111267... OKAY!
;; > (sqrt-iter 1 0.01)
;; 0.1003257851096060565971421... OKAY!
;; > (sqrt-iter 1 0.001)
;; 0.0412454260749911504584959... FAILED
;; > (sqrt-iter 1 0.0001)
;; 0.0323084483304812185178363... FAILED
;; > (sqrt-iter 1 0.00001)
;; 0.0313564901077171562326724... FAILED
;;>
;; The last one should have two zeroes after the decimal point

;; For large values, it may get too much time to the guess reach the big value, and sometimes
;; the absolute value of the squared guess and x never turn to be less than 0.001.
;; As far as we concerned there is large value, when the guess is improved we are squaring the
;; half of this big value summed up with the initial guess, and it might cause overflow due
;; squaring a very big number.
;; Also, the machine precision may not be able to represent a small difference in a big number.
;; The small diffences end up to be lost

;; (improve 1 1000000000000)
;; 500000000000.5
;; square 500000000000.5)
;; 250000000000500000000000.25
;; > (improve 500000000000.5 1000000000000)
;; 250000000001.2499999999990000000000009...
;; > 
;; > (improve 250000000001.2499999999990000000000009...
;; 1000000000000)
;; 125000000002.6249999999895000000000584...


(define (square x)
  ( * x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ ( + x y) 2))

(define (good-enough? guess x)
  (< (abs-error guess x) 0.001))

(define (abs-error guess x)
  (abs (/ (- guess (improve guess x)) guess)))
