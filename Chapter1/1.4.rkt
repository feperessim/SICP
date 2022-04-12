(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; There are two scenarios, whether b are negative or positive, lets analise both.
;; Let a be 1 and b be 1. So aplying the procedure a-plus-abs-b to the arguments 1 and 1 we have that:
;; (a-plus-abs-b 1 1)
;; (if (> 1 0) + -) 1 1)
;; (if (#true) + -) a b)
;; (+ 1 1)
;; 2
;; Now let a to be 1 again and b to be -1
;; (a-plus-abs-b 1 -1)
;; (if (> -1 0) + -) 1 -1)
;; (if (#false) + -) 1 -1)
;; (- 1 -1)
;; 2


