(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list a1 '+ a2))))

(define (make-product m1 m2) 
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list m1 '* m2))))

(define (make-exponentiation base exponent) 
  (cond ((=number? exponent 0) 1)
	((=number? exponent 1) base)
	((and (number? base) (number? exponent)) (expt base exponent))
	(else (list base '** exponent))))

(define (sum? x)
  (and (pair? x) (memq '+ x)))
  
(define (addend s)
  (let ((first-term (extract-addend s)))
    (if (single-term? first-term)
	(car first-term)
	first-term)))

(define (augend s)
  (let ((second-term (extract-augend s)))
    (if (single-term? second-term)
	(car second-term)
	second-term)))
    
(define (single-term? s)
  (and (pair? s) (null? (cdr s))))

(define (extract-addend s)
  (if (or (null? s) (eq? (car s) '+))
      '()
      (cons (car s) (extract-addend (cdr s)))))
      
(define (extract-augend s)
  (cdr (memq '+ s)))

(define (product? x)
  (and (pair? x) (memq '* x)))

(define (exponentiation? x)
  (and (pair? x) (memq '** x)))

(define (base b)
  (let ((first-term (extract-base b)))
    (if (single-term? first-term)
	(car first-term)
	first-term)))

(define (exponent e)
  (let ((second-term (extract-exponent e)))
    (if (single-term? second-term)
	(car second-term)
	second-term)))

(define (extract-base b)
  (if (or (null? b) (eq? (car b) '**))
      '()
      (cons (car b) (extract-base (cdr b)))))
      
(define (extract-exponent e)
  (cdr (memq '** e)))

(define (multiplier p)
  (let ((first-term (extract-multiplier p)))
    (if (single-term? first-term)
	(car first-term)
	first-term)))

(define (multiplicand p)
  (let ((second-term (extract-multiplicand p)))
    (if (single-term? second-term)
	(car second-term)
	second-term)))

(define (extract-multiplier p)
  (if (or (null? p) (eq? (car p) '*))
      '()
      (cons (car p) (extract-multiplier (cdr p)))))
      
(define (extract-multiplicand p)
  (cdr (memq '* p)))

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	((exponentiation? exp)
	 (make-product (exponent exp)
		       (make-product  (make-exponentiation (base exp) (- (exponent exp) 1))
				      (deriv (base exp) var))))
	(else
	 (error "unknown expression type -- DERIV" exp))))
