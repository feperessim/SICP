(define (equal? l1 l2)
  (cond ((and (null? l1)
	      (null? l2))
	 true)
	((and (pair? l1)
	      (pair? l2))
	 (and (equal? (car l1)
		      (car l2))
	      (equal? (cdr l1)
		      (cdr l2))))
	(else (eq? l1 l2))))
	 
	      
;; Test cases
(equal? '(this is a list)  ;; #t
        '(this is a list))

(equal? '(this is a list) ;; #f
        '(this (is a) list))

(equal? '(this (is a) list) 
        '(this (is a) list)) ;; #t

(equal? '(this (is a) list) 
        '(this (is a))) ;; #t
 
