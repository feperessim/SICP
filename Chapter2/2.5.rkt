(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car z)
  (extract-expt z 2))

(define (cdr z)
  (extract-expt z 3))

(define (extract-expt n factor)
  (define (iter n factor counter)
    (if (not (= (remainder n factor) 0))
	     counter
	     (iter (/ n factor) factor (+ counter 1))))
  (iter n factor 0))

(define pair (cons 7 8))
(car pair)
(cdr pair)

