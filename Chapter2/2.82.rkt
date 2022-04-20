(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
	  (let ((coerced-args (coerce-args args type-tags)))
	    (if (null? coerced-args)
		(error 
		 "No method for these types"
		 (list op type-tags))
		(apply-generic op coerced-args)))))))


(define (coerce-args args type-tags)
  (define (coercion  args type-tags)
    (if (null? args)
	'()
	(let ((type1 (car type-tags))
	      (arg1 (car args)))
	  (let ((result (map (lambda (arg2 type2)
			       (coerce arg1 type1 arg2 type2))
			     args type-tags)))
	    (if (member? '() result)
		'()
		(coercion  (cdr args) (cdr type-tags)))))))
  (coercion  args type-tags))

  
  (define (coerce  arg1 arg2 type1 type2)
    (if (equal? type1 type2)
	arg2
	(let ((t1->t2 (get-coercion type1 type2)))
	  (if t1->t2
	      arg2
	      '()))))

;; Suppose we have the following tower of types: A->B->C
;; Suppose our list of arguments have the following types: (B A B C)
;; At the end of the process every element would be coerced to C yeildin (C C C C)
;;  but is in the table there isn't an entry for type  (C C) for instance,  it will not be
;; able to coerce the types. This problem is simila to the previous exercise.
