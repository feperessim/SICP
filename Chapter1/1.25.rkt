(define (expmod base exp m)
  (remainder (fast-expt base ep) m))

;; Yes, she is correct.
;; The remainder of the product/square of all remainders is the same as the remainder of all products.
;; Thus with her procedure, the remainder procedure is called only once instead, and the number get raised to the nth power..
;; As said in the footnote the original procedure expmod is useful, because you can acomplish the computation
;; without deal with numbers larger than m, thus in every time the procedure is applied the ramainder is taken
;; and it does not let the number become large saving computer resources with that.