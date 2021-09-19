(define (adjoin-set x set)
  (if (null? set)
      (cons x set)
      (let ((x1 (car set)))
	(cond ((= x1 x) set)
	      ((< x1 x) (cons x1 (adjoin-set x (cdr set))))
	      (else (append (list x x1) (cdr set)))))))
		    
