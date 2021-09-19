;;;;;;;;;;;;;;;;;;;
;;      a        ;;
;;;;;;;;;;;;;;;;;;;

;; We modularize the derive procedure in such a way it searchs in the table
;; the correspond procedure associated with the deriv operation and the
;; op implementations. Thus, the derive procedure is not dependent on the
;; implementation os the operation, as well, the procedure is now clean
;; coded and easy to read. In summary, after getting the appropiated
;; procedure from the table, the opertors are applied to the operands and
;; the result of derivate a expression is yielded.

;; We can't assimilate number? and variable? into the data-directed dispatch
;; because they are predicates that perform tests on primitive data types.

;;;;;;;;;;;;;;;;;;;
;;      b        ;;
;;;;;;;;;;;;;;;;;;;

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	(else ((get 'deriv (operator exp)) (operands exp)
	       var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (install-sum-package)
  ;; internal procedures
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
	  ((=number? a2 0) a1)
	  ((and (number? a1) (number? a2)) (+ a1 a2))
	  (else (list '+ a1 a2))))

  (define (addend s) (cadr s))

  (define (augend s) (caddr s))
  
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
	      (deriv (augend exp) var)))
  ;; interface to the rest of the system
  (put 'deriv '+ deriv-sum)

  'done)


(define (install-product-package)
  ;; internal procedures
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	  ((=number? m1 1) m2)
	  ((=number? m2 1) m1)
	  ((and (number? m1) (number? m2)) (* m1 m2))
	  (else (list '* m1 m2))))

  (define (multiplier p) (cadr p))

  (define (multiplicand p) (caddr p))
  
  (define (deriv-product exp var)
    (make-sum
     (make-product (multiplier exp)
		   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
		   (multiplicand exp))))
  ;; interface to the rest of the system
  (put 'deriv '* deriv-product)

  'done)

;;;;;;;;;;;;;;;;;;;
;;      c        ;;
;;;;;;;;;;;;;;;;;;;

(define (install-exponentiation-package)
  ;; internal procedures
  (define (make-exponentiation base exponent) 
  (cond ((=number? exponent 0) 1)
	((=number? exponent 1) base)
	((and (number? base) (number? exponent)) (expt base exponent))
	(else (list '** base exponent))))

  (define (base s)  (cadr s))

  (define (exponent s)  (caddr s))
  
  (define (deriv-exponentiation exp var)
    (make-product (exponent exp)
		  (make-product
		   (make-exponentiation (base exp) (- (exponent exp) 1))
		   (deriv (base exp) var))))
  ;; interface to the rest of the system
  (put 'deriv '** deriv-exponentiation)

  'done)


;;;;;;;;;;;;;;;;;;;
;;      d        ;;
;;;;;;;;;;;;;;;;;;;

;; We would have to change the way the table
;; indexes the the operator and the type. To do so,
;; we only need to change the order in which
;; the operator and the type are in the put
;; procedure. Thus we'd need to change all
;; put statements.
