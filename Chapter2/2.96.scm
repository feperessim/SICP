(use-modules (srfi srfi-1))

(define operation-table (make-hash-table))

(define (put op types proc)
  (hash-set! operation-table (cons op types) proc))

(define (get op types)
  (hash-ref operation-table (cons op types) #f))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

(define type-level-table (make-hash-table))

(define (put-type-level type level)
  (hash-set! type-level-table type level))

(define (get-type-level type)
  (hash-ref type-level-table type #f))

(define (type-level type)
  (get-type-level type))

(define (type-higher? type1 type2)
  (> (type-level type1) (type-level type2)))

(define (highest-type types)
  (if (null? (cdr types))
      (car types)
      (let ((rest-highest (highest-type (cdr types))))
        (if (type-higher? (car types) rest-highest)
            (car types)
            rest-highest))))

(define (raise arg) (apply-generic 'raise arg))
(define (equ? x y) (apply-generic 'equ? x y))
(define (project x) (apply-generic 'project x))

;; Coercion by successive raising
(define (coerce arg target-type)
  (if (eq? (type-tag arg) target-type)
      arg
      (let ((raised-arg (raise arg)))
        (if raised-arg
            (coerce raised-arg target-type)
            #f))))

;; Drop procedure
(define (drop obj)
  (let ((proj-proc (get 'project (list (type-tag obj)))))
    (if proj-proc
        (let ((projected-obj (proj-proc (contents obj))))
          (if projected-obj
              (let ((raised-back (raise projected-obj)))
                (if (and raised-back (equ? obj raised-back))
                    (drop projected-obj)  ; Recursively drop further
                    obj))                 ; Can't drop, return original
              obj))                       ; No projection possible
        obj)))                            ; No project operation


;; apply-generic with drop
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (let ((result (apply proc (map contents args))))
            ;; Apply drop to simplify the result, but only for arithmetic operations
            (if (memq op '(add sub mul div))
                (drop result)
	        result))
          ;; Coercion logic for when direct operation not found
          (if (= (length args) 1)
              (car args)  ; Single argument, return as-is
              (let ((highest-type-tag (highest-type type-tags)))
                (let ((coerced-args 
                       (map (lambda (obj) (coerce obj highest-type-tag)) args)))
                  (if (every identity coerced-args)
                      (apply apply-generic (cons op coerced-args))
                      (error "Could not coerce args" (list op type-tags))))))))))

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))
  (put 'gcd '(scheme-number scheme-number)
       (lambda (x y) (tag (gcd x y))))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'negate '(scheme-number)
       (lambda (x) (tag (- x))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (add (tag x) (negate (tag y)))))
  (put 'equ? '(scheme-number scheme-number) =)
  'done)


(define (order term) (car term))
(define (coeff term) (cadr term))
(define (make-term order coeff)
  (list order coeff))


(define (install-dense-term-list-package)
  ;; Need to be generic
  (define (first-term term-list)
    (make-term (- (length term-list) 1)
               (car term-list)))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list)
    (null? term-list))
  ;; Works only internally so no need to put in generic package
  (define (make-term-list-with-zeros coeff order current-length)
    (let ((size (- order current-length)))
      (if (= size 0)
          (list coeff)
          (cons coeff
                (make-term-list-with-zeros
                 (make-scheme-number 0)
                 order
                 (+ current-length 1))))))
  ;; Since it works over term-list it need to be put into the generic package
  (define (adjoin-term term term-list)
    (cond ((=zero? (coeff term)) term-list)
          ((< (order term) (length term-list))
           (error "Term order <= term-list:
                  ADJOIN TERM"))
          (else           
           (append
            (make-term-list-with-zeros
             (coeff term)
             (order term)
             (length term-list))
            term-list))))
  (define (make-term-list term-list)
    (tag term-list))
  ;; interface to rest of the system
  (define (tag L) (attach-tag 'dense-term-list L))
  (put 'first-term '(dense-term-list)
       (lambda (term-list) (first-term term-list)))
  (put 'rest-terms '(dense-term-list)
       (lambda (term-list) (tag (rest-terms term-list))))
  (put 'empty-termlist? '(dense-term-list)
       (lambda (term-list) (empty-termlist? term-list)))
  (put 'adjoin-term '(dense-term-list)
       (lambda (term term-list)
         (tag (adjoin-term term term-list))))
  (put 'make-term-list 'dense-term-list
       (lambda (term-list) (make-term-list term-list))))


(define (install-sparse-term-list-package)
  ;; Need to be generic
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list)
    (null? term-list))
  ;; Works only internally so no need to put in generic package
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (make-term-list term-list)
    (tag term-list))
  ;; interface to rest of the system
  (define (tag L) (attach-tag 'sparse-term-list L))
  (put 'first-term '(sparse-term-list)
       (lambda (term-list) (first-term term-list)))
  (put 'rest-terms '(sparse-term-list)
       (lambda (term-list) (tag (rest-terms term-list))))
  (put 'empty-termlist? '(sparse-term-list)
       (lambda (term-list) (empty-termlist? term-list)))
  (put 'adjoin-term '(sparse-term-list)
       (lambda (term term-list)
         (tag (adjoin-term term term-list))))
  (put 'make-term-list 'sparse-term-list
       (lambda (term-list) (make-term-list term-list))))


(define (install-polynomial-package)
  (define (make-poly variable term-list)
    (define (remove-leading-zeros tl)
      (cond ((empty-termlist? tl) tl)
            ((=zero? (coeff (first-term tl)))
             (remove-leading-zeros (rest-terms tl)))
            (else tl)))             
    (cons variable (remove-leading-zeros term-list)))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1)
         (variable? v2)
         (eq? v1 v2)))
  (define (adjoin-term term term-list)
    (let ((proc (get 'adjoin-term (list (type-tag term-list)))))
      (if proc
          (proc term (contents term-list))
          (error "No adjoin-term for type" (type-tag term-list)))))
  (define (first-term term-list)
    (apply-generic 'first-term term-list))
  (define (rest-terms term-list)
    (apply-generic 'rest-terms term-list))
  (define (empty-termlist? term-list)
    (apply-generic 'empty-termlist? term-list))  
  (define (=zero-poly? p)
    (or (empty-termlist? (term-list p))
        (and (=zero? (coeff
                      (first-term
                       (term-list p))))
             (=zero-poly? (make-poly
                           (variable p)
                           (rest-terms
                            (term-list p)))))))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (make-poly
         (variable p1)
         (add-terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var:
               ADD-POLY"
               (list p1 p2))))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1
                     (add-terms (rest-terms L1)
                                L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2
                     (add-terms
                      L1
                      (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term
                      (order t1)
                      (add (coeff t1)
                           (coeff t2)))
                     (add-terms
                      (rest-terms L1)
                      (rest-terms L2)))))))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (make-poly
         (variable p1)
         (mul-terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var:
              MUL-POLY"
               (list p1 p2))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        L1
        (add-terms 
         (mul-term-by-all-terms 
          (first-term L1) L2)
         (mul-terms (rest-terms L1) L2))))  
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        L
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term 
            (+ (order t1) (order t2))
            (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms 
            t1 
            (rest-terms L))))))
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (map
         (lambda (termlist)
           (make-poly (variable p1) termlist))         
         (div-terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var:
              DIV-POLY"
               (list p1 p2))))
  (define (empty-termlist-of-type L)
    ((get 'make-term-list (type-tag L)) '()))
  (define (div-terms L1 L2)
    (let ((E (empty-termlist-of-type L2)))
      (if (empty-termlist? L1)
          (list E E)
          (let ((t1 (first-term L1))
                (t2 (first-term L2)))
            (if (> (order t2) (order t1))
                (list E L1)
                (let ((new-c (div (coeff t1) 
                                  (coeff t2)))
                      (new-o (- (order t1) 
                                (order t2))))
                  (let ((rest-of-result
                         (div-terms
                          (sub-terms L1
                                     (mul-term-by-all-terms
                                      (make-term new-o new-c)
                                      L2))
                          L2)))
                    (list
                     (adjoin-term
                      (make-term new-o new-c)
                      (car rest-of-result))
                     (cadr rest-of-result)))))))))
  (define (negate-poly p)
    (make-poly (variable p)
               (negate-terms (term-list p))))
  (define (negate-terms L)-
    (if (empty-termlist? L)
        L
        (let ((t (first-term L)))
          (adjoin-term
           (make-term
            (order t)
            (negate (coeff t)))
           (negate-terms (rest-terms L))))))
  (define (sub-poly p1 p2)
    (tag (add-poly p1 (negate-poly p2))))
  (define (sub-terms L1 L2)
    (add-terms L1 (negate-terms L2)))
  (define (equ-poly? p1 p2)
    (and (same-variable? (variable p1) (variable p2))
         (equ-termlist? (term-list p1) (term-list p2))))
  (define (equ-termlist? L1 L2)
    (cond ((and (empty-termlist? L1) (empty-termlist? L2)) #t)
          ((or  (empty-termlist? L1) (empty-termlist? L2)) #f)
          (else
           (and (= (order (first-term L1)) (order (first-term L2)))
                (equ? (coeff (first-term L1))
                      (coeff (first-term L2)))
                (equ-termlist? (rest-terms L1) (rest-terms L2))))))
  (define (remainder-terms L1 L2)
    (cadr (div-terms L1 L2)))
  (define (pseudoremainder-terms P Q)
    (let ((O1 (order (first-term P)))
          (O2 (order (first-term Q)))
          (c (coeff (first-term Q))))
      (let ((factor (expt (cdr c) (+ 1 (- O1 O2)))))
        (let ((factor-x-p
               (mul-term-by-all-terms
                (make-term 0 (make-scheme-number factor))
                P)))         
          (remainder-terms factor-x-p Q)))))
  (define (gcd-coeffs c L)
    (if (empty-termlist? L)
        c
        (gcd-coeffs
         (greatest-common-divisor c
                                  (coeff (first-term L)))
         (rest-terms L))))
  (define (remove-common-factors L)
    (if (empty-termlist? L)
        L
        (let ((gcd-coeff
               (gcd-coeffs (coeff (first-term L))
                           (rest-terms L))))
          (let ((divisor
                 (adjoin-term (make-term 0 gcd-coeff)
                              (empty-termlist-of-type L))))
            (car (div-terms L divisor))))))
  (define (gcd-terms a b)
    (if (empty-termlist? b)
        (remove-common-factors a)
        (gcd-terms b (pseudoremainder-terms a b))))
  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (gcd-terms (term-list p1)
                   (term-list p2))
        (error "Polys not in same var:
              GCD-POLY"
               (list p1 p2))))
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) 
         (tag (make-poly var terms))))
  (put 'make-term 'polynomial 
       (lambda (order coeff) (make-term order coeff)))
  (put '=zero? '(polynomial)       
       (lambda (p) (=zero-poly? p)))
  (put 'negate '(polynomial)
       (lambda (p) (tag (negate-poly p))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (sub-poly p1 p2)))
  (put 'equ? '(polynomial polynomial) equ-poly?)
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) 
         (map tag (div-poly p1 p2))))
  (put 'gcd '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (cons (variable p1) (gcd-poly p1 p2)))))
  'done)

(define (is-empty-term-list? term-list)
  ((get 'empty-termlist? 'dense-term-list) term-list))

(define (make-dense-term-list term-list)
  ((get 'make-term-list 'dense-term-list) term-list))

(define (make-sparse-term-list terms)
  ((get 'make-term-list 'sparse-term-list) terms))
 
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

;; Defining generic operations
(define (=zero? z)
  (apply-generic '=zero? z))
(define (greatest-common-divisor x y)
  (apply-generic 'gcd x y))
(define (add x y) (apply-generic 'add x y))
(define (mul x y) (apply-generic 'mul x y))
(define (sub x y) (apply-generic 'sub x y))
(define (div x y) (apply-generic 'div x y))
(define (negate x) (apply-generic 'negate x))

;; Constructor for scheme numbers
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (first-term tl) (apply-generic 'first-term tl))
(define (empty-termlist? tl) (apply-generic 'empty-termlist? tl))

;; Rational package
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
      (cons n d))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

;; Utility functions for rational numbers
(define (numer x) (car x))
(define (denom x) (cdr x))

;; test cases
(install-scheme-number-package)
(install-dense-term-list-package)
(install-sparse-term-list-package)
(install-polynomial-package)
(install-rational-package)

;; Set up the tower levels
(put-type-level 'scheme-number 1)
(put-type-level 'rational 2)
(put-type-level 'polynomial 2)
(put-type-level 'dense-term-list 3)

(put 'equ? '(rational rational)
     (lambda (x y) (and (equ? (numer x) (numer y))
                        (equ? (denom x) (denom y)))))

;; Install raise operations
;; This is problematic for when the polynomial is in a different variable
(put 'raise '(scheme-number)
     (lambda (arg) (make-polynomial 'x (list (make-scheme-number arg)))))

;; Install project operations
(put 'project '(polynomial)
     (lambda (z)
       (let ((term-list (cdr z)))  ; term-list COM tag
         (if (empty-termlist? term-list)
             (make-scheme-number 0)
             (coeff (first-term term-list))))))

;; Test suit
;; Assertion helper
(define (assert-equal actual expected message)
  (if (eq? actual expected)
      (display (string-append "PASS: " message "\n"))
      (begin
        (display (string-append "FAIL: " message "\n"))
        (display "Expected: ") (display expected) (newline)
        (display "Actual: ") (display actual) (newline))))

(define (assert-equ actual expected message)
  (if (equ? actual expected)
      (display (string-append "PASS: " message "\n"))
      (begin
        (display (string-append "FAIL: " message "\n"))
        (display "Expected: ") (display expected) (newline)
        (display "Actual: ") (display actual) (newline))))

(define (quotient-of div-result)
  (car div-result))

(define (remainder-of div-result)
  (cadr div-result))

;; Convenience constructors
(define (num n) (make-scheme-number n))

;; Testing dense term list alone
(define (poly-dense terms) (make-polynomial 'x (make-dense-term-list terms)))

(define dense-poly-x
  (make-polynomial 'x
                   (make-dense-term-list
                    (list (make-scheme-number 3)
                          (make-scheme-number 2)
                          (make-scheme-number 1)))))
(define dense-poly-y
  (make-polynomial 'x
                   (make-dense-term-list
                    (list (make-scheme-number 4)
                          (make-scheme-number 3)
                          (make-scheme-number 2)))))
(define dense-poly-u
  (make-polynomial 'x
                   (make-dense-term-list
                    (list (make-scheme-number 4)
                          (make-scheme-number 3)))))

(define dense-poly-w
  (make-polynomial 'x
                   (make-dense-term-list
                    (list (make-scheme-number 1)
                          (make-scheme-number 1)
                          (make-scheme-number 0)))))

(define dense-poly-zero
  (make-polynomial 'x
                   (make-dense-term-list
                    (list (make-scheme-number 0)
                          (make-scheme-number 0)
                          (make-scheme-number 0)))))
(define dense-poly-g
  (make-polynomial 'x
                   (make-dense-term-list
                    (list (make-scheme-number 4)
                          (make-scheme-number 3)
                          (make-scheme-number 0)
                          (make-scheme-number 1)))))

(define dense-poly-empty (make-polynomial 'x (make-dense-term-list '())))



;; 1. Negation tests
(assert-equ (negate (num 2))
            (num -2)
            "Negate scheme-number")

(assert-equ (negate dense-poly-y) 
            (poly-dense (list (num -4) (num -3) (num -2)))
            "Negate dense-poly-y")

(assert-equ (negate dense-poly-zero)
            dense-poly-empty
            "Negate dense-poly-zero produces empty polynomial")

;; 2. Addition tests
(assert-equ (add dense-poly-x dense-poly-zero)
            dense-poly-x
            "Add dense-poly-x to poly-zero produces dense-poly-x")

(assert-equ (add dense-poly-x dense-poly-empty)
            dense-poly-x
            "Add dense-poly-x to poly-empty produces dense-poly-x")

(assert-equ (add dense-poly-x dense-poly-x)
            (poly-dense (list (num 6) (num 4) (num 2)))
            "Add dense-poly-x to itself produces 2 times dense-poly-x")

(assert-equ (add dense-poly-x dense-poly-w)
            (poly-dense (list (num 4) (num 3) (num 1)))
            "Add dense-poly-x to poly-w")

(assert-equ (add dense-poly-x dense-poly-g)
            (poly-dense (list (num 4) (num 6) (num 2) (num 2)))
            "Add dense-poly-x to dense-poly-g")

;; 3. Subtraction tests
(assert-equ (sub dense-poly-x dense-poly-zero)
            dense-poly-x
            "Subtract dense-poly-x from dense-poly-zero produces dense-poly-x")

(assert-equ (sub dense-poly-x dense-poly-empty)
            dense-poly-x
            "Subtract dense-poly-x from dense-poly-empty produces dense-poly-x")

(assert-equ (sub dense-poly-x dense-poly-x)
            dense-poly-empty
            "Subtract dense-poly-x from itself produces empty dense-polynomial")

(assert-equ (sub dense-poly-x dense-poly-w)
            (poly-dense (list (num 2) (num 1) (num 1)))
            "Subtract dense-poly-w from dense-poly-x")

(assert-equ (sub dense-poly-w dense-poly-x)
            (poly-dense (list (num -2) (num -1) (num -1)))
            "Subtract dense-poly-x from dense-poly-w")


;; 4. Multiplication tests
(assert-equ (mul dense-poly-x dense-poly-y)
            (poly-dense (list (num 12) (num 17) (num 16) (num 7) (num 2)))
            "Multiply dense-poly-x by dense-poly-y")

(assert-equ (mul dense-poly-x dense-poly-u)
            (poly-dense (list (num 12) (num 17) (num 10) (num 3)))
            "Multiply dense-poly-x by dense-poly-u")

(assert-equ (mul dense-poly-x dense-poly-empty)
            dense-poly-empty
            "Multiply dense-poly-x by empty dense-polynomial produces empty dense-polynomial")

(assert-equ (mul dense-poly-x dense-poly-zero)
            dense-poly-empty
            "Multiply dense-poly-x by zero dense-polynomial produces empty dense-polynomial")


;; 5. =zero? tests
(assert-equal (=zero? dense-poly-empty) #t "dense-poly-empty is zero")
(assert-equal (=zero? dense-poly-x) #f "dense-poly-x is not zero")
(assert-equal (=zero? (negate dense-poly-zero)) #t "negate dense-poly-zero is zero")

;; 6. Division tests
(let ((div-result
       (div dense-poly-g dense-poly-u)))
  (assert-equ
   (quotient-of div-result)
   (poly-dense (list (num 1) (num 0) (num 0)))
   "Divide dense-poly-g by dense-poly-u (dense/dense division) - quotient")

  (assert-equ
   (remainder-of div-result)
   (poly-dense (list (num 1)))
   "Divide dense-poly-g by dense-poly-u (dense/dense division) - remainder"))

(let ((div-result
       (div dense-poly-x dense-poly-y)))
  (assert-equ
   (quotient-of div-result)
   (poly-dense (list (num (/ 3 4))))
   "Divide dense-poly-x by dense-poly-y (dense/dense division) - quotient")

  (assert-equ
   (remainder-of div-result)
   (poly-dense (list (num (/ -1 4)) (num (/ -2 4))))
   "Divide dense-poly-x by dense-poly-y (dense/dense division) - remainder"))

(let ((div-result
       (div dense-poly-zero dense-poly-x)))
  (assert-equ
   (quotient-of div-result)
   dense-poly-empty
   "Divide dense-poly-zero by dense-poly-x (dense/dense division) - quotient")

  (assert-equ
   (remainder-of div-result)
   dense-poly-empty
   "Divide dense-poly-zero by dense-poly-x (dense/dense division) - remainder"))

(let ((div-result
       (div dense-poly-empty dense-poly-x)))
  (assert-equ
   (quotient-of div-result)
   dense-poly-empty
   "Divide dense-poly-empty by dense-poly-x (dense/dense division) - quotient")

  (assert-equ
   (remainder-of div-result)
   dense-poly-empty
   "Divide dense-poly-empty by dense-poly-x (dense/dense division) - remainder"))

(let ((div-result
       (div dense-poly-y dense-poly-y)))
  (assert-equ
   (quotient-of div-result)
   (poly-dense (list (num 1)))
   "Divide dense-poly-y by dense-poly-y (dense/dense division) - quotient")

  (assert-equ
   (remainder-of div-result)
   dense-poly-empty
   "Divide dense-poly-y by dense-poly-y (dense/dense division) - remainder"))


;; Testing sparse term list alone
;; Testing cases for sparse term list representation only
(define (poly-sparse terms) (make-polynomial 'x (make-sparse-term-list terms)))

(define sparse-poly-x
  (make-polynomial 'x
                   (make-sparse-term-list
                    (list (make-term 2 (num 3))
                          (make-term 1 (num 2))
                          (make-term 0 (num 1))))))

(define sparse-poly-y
  (make-polynomial 'x
                   (make-sparse-term-list
                    (list (make-term 2 (num 4))
                          (make-term 1 (num 3))
                          (make-term 0 (num 2))))))

(define sparse-poly-u
  (make-polynomial 'x
                   (make-sparse-term-list
                    (list (make-term 1 (num 4))
                          (make-term 0 (num 3))))))

(define sparse-poly-w
  (make-polynomial 'x
                   (make-sparse-term-list
                    (list (make-term 2 (num 1))
                          (make-term 1 (num 1))
                          (make-term 0 (num 0))))))

(define sparse-poly-zero
  (make-polynomial 'x
                   (make-sparse-term-list
                    (list (make-term 2 (num 0))
                          (make-term 1 (num 0))
                          (make-term 0 (num 0))))))

(define sparse-poly-g
  (make-polynomial 'x
                   (make-sparse-term-list
                    (list (make-term 3 (num 4))
                          (make-term 2 (num 3))
                          (make-term 0 (num 1))))))

(define sparse-poly-empty 
  (make-polynomial 'x (make-sparse-term-list '())))


;; 1. Negation tests
(assert-equ (negate (num 2))
            (num -2)
            "Negate scheme-number")

(assert-equ (negate sparse-poly-y) 
            (poly-sparse
             (list
              (make-term 2 (num -4))
              (make-term 1 (num -3))
              (make-term 0 (num -2))))
            "Negate sparse-poly-y")

(assert-equ (negate sparse-poly-zero)
            sparse-poly-empty
            "Negate sparse-poly-zero produces empty polynomial")

;; 2. Addition tests
(assert-equ (add sparse-poly-x sparse-poly-zero)
            sparse-poly-x
            "Add sparse-poly-x to  sparse-poly-zero produces poly-x")

(assert-equ (add sparse-poly-x sparse-poly-empty)
            sparse-poly-x
            "Add sparse-poly-x to sparse-poly-empty produces poly-x")

(assert-equ (add sparse-poly-x sparse-poly-x)
            (poly-sparse
             (list
              (make-term 2 (num 6))
              (make-term 1 (num 4))
              (make-term 0 (num 2))))
            "Add  sparse-poly-x to itself produces 2 time sparse-poly-x")

(assert-equ (add sparse-poly-x sparse-poly-w)
            (poly-sparse
             (list
              (make-term 2 (num 4))
              (make-term 1 (num 3))
              (make-term 0 (num 1))))
            "Add sparse-poly-x to sparse-poly-w")

(assert-equ (add sparse-poly-x sparse-poly-g)
            (poly-sparse
             (list
              (make-term 3 (num 4))
              (make-term 2 (num 6))
              (make-term 1 (num 2))
              (make-term 0 (num 2))))
            "Add sparse-poly-x to sparse-poly-g")

;; 3. Subtraction tests
(assert-equ (sub sparse-poly-x sparse-poly-zero)
            sparse-poly-x
            "Subtract sparse-poly-x from sparse-poly-zero produces sparse-poly-x")

(assert-equ (sub sparse-poly-x sparse-poly-empty)
            sparse-poly-x
            "Subtract sparse-poly-x from sparse-poly-empty produces sparse-poly-x")

(assert-equ (sub sparse-poly-x sparse-poly-x)
            sparse-poly-empty
            "Subtract sparse-poly-x from itself produces empty polynomial")

(assert-equ (sub sparse-poly-x sparse-poly-w)
            (poly-sparse
             (list
              (make-term 2 (num 2))
              (make-term 1 (num 1))
              (make-term 0 (num 1))))
            "Subtract sparse-poly-w from sparse-poly-x")


(assert-equ (sub sparse-poly-w sparse-poly-x)
            (poly-sparse
             (list
              (make-term 2 (num -2))
              (make-term 1 (num -1))
              (make-term 0 (num -1))))
            "Subtract sparse-poly-w from sparse-poly-x")

;; 4. Multiplication tests
(assert-equ (mul sparse-poly-x sparse-poly-y)
            (poly-sparse
             (list
              (make-term 4 (num 12))
              (make-term 3 (num 17))
              (make-term 2 (num 16))
              (make-term 1 (num 7))
              (make-term 0 (num 2))))
            "Multiply sparse-poly-x by sparse-poly-y")

(assert-equ (mul sparse-poly-x sparse-poly-u)
            (poly-sparse
             (list
              (make-term 3 (num 12))
              (make-term 2 (num 17))
              (make-term 1 (num 10))
              (make-term 0 (num 3))))
            "Multiply sparse-poly-x by sparse-poly-u")

(assert-equ (mul sparse-poly-x sparse-poly-empty)
            sparse-poly-empty
            "Multiply sparse-poly-x by empty polynomial produces empty polynomial")

(assert-equ (mul sparse-poly-x sparse-poly-zero)
            sparse-poly-empty
            "Multiply sparse-poly-x by zero polynomial produces empty polynomial")


;; 5. =zero? tests
(assert-equal (=zero? sparse-poly-empty) #t "sparse-poly-empty is zero")
(assert-equal (=zero? sparse-poly-x) #f "sparse-poly-x is not zero")
(assert-equal (=zero? (negate sparse-poly-zero)) #t "negate sparse-poly-zero is zero")

;; 6. Division tests
(let ((div-result
       (div sparse-poly-g sparse-poly-u)))
  (assert-equ
   (quotient-of div-result)
   (poly-sparse (list (make-term 2 (num 1))))
   "Divide sparse-poly-g by sparse-poly-u (sparse/sparse division) - quotient")

  (assert-equ
   (remainder-of div-result)
   (poly-sparse (list (make-term 0 (num 1))))
   "Divide sparse-poly-g by  sparse-poly-u (sparse/sparse division) - remainder"))

(let ((div-result
       (div sparse-poly-x sparse-poly-y)))
  (assert-equ
   (quotient-of div-result)
   (poly-sparse (list (make-term 0 (num (/ 3 4)))))
   "Divide sparse-poly-x by sparse-poly-y (sparse/sparse division) - quotient")

  (assert-equ
   (remainder-of div-result)
   (poly-sparse (list
                 (make-term 1 (num (/ -1 4)))
                 (make-term 0 (num (/ -2 4)))))
   "Divide sparse-poly-x by sparse-poly-y (sparse/sparse division) - remainder"))

(let ((div-result
       (div sparse-poly-zero sparse-poly-x)))
  (assert-equ
   (quotient-of div-result)
   sparse-poly-empty
   "Divide sparse-poly-zero by sparse-poly-x (sparse/sparse division) - quotient")

  (assert-equ
   (remainder-of div-result)
   sparse-poly-empty
   "Divide sparse-poly-zero by sparse-poly-x (sparse/sparse division) - remainder"))

(let ((div-result
       (div sparse-poly-empty sparse-poly-x)))
  (assert-equ
   (quotient-of div-result)
   sparse-poly-empty
   "Divide sparse-poly-empty by sparse-poly-x (sparse/sparse division) - quotient")

  (assert-equ
   (remainder-of div-result)
   sparse-poly-empty
   "Divide sparse-poly-empty by sparse-poly-x (sparse/sparse division) - remainder"))

(let ((div-result
       (div sparse-poly-y sparse-poly-y)))
  (assert-equ
   (quotient-of div-result)
   (poly-sparse (list
                 (make-term 0 (num 1))))
   "Divide sparse-poly-y by sparse-poly-y (sparse/sparse division) - quotient")

  (assert-equ
   (remainder-of div-result)
   sparse-poly-empty
   "Divide sparse-poly-y by sparse-poly-y (sparse/sparse division) - remainder"))


;; Testing sparse and dense term list mixed
;; 2. Addition tests
(assert-equ (add sparse-poly-x dense-poly-zero)
            sparse-poly-x
            "Add  sparse-poly-x to dense-poly-zero produces  sparse-poly-x")

(assert-equ (add dense-poly-zero sparse-poly-x)
            sparse-poly-x
            "Add dense-poly-zero to sparse-poly-x produces sparse-poly-x")

(assert-equ (add sparse-poly-x dense-poly-x)
            (poly-dense
             (list
              (num 6)
              (num 4)
              (num 2)))
            "Add sparse-poly-x to dense-poly-x produces 2 times dense-poly-x")

(assert-equ (add dense-poly-x sparse-poly-x)
            (poly-sparse
             (list
              (make-term 2 (num 6))
              (make-term 1 (num 4))
              (make-term 0 (num 2))))
            "Add sparse-poly-x to dense-poly-x produces 2 times sparse-poly-x")

(assert-equ (add sparse-poly-x dense-poly-w)
            (poly-sparse
             (list
              (make-term 2 (num 4))
              (make-term 1 (num 3))
              (make-term 0 (num 1))))
            "Add sparse-poly-x to dense-poly-w")

(assert-equ (add dense-poly-x sparse-poly-g)
            (poly-sparse
             (list
              (make-term 3 (num 4))
              (make-term 2 (num 6))
              (make-term 1 (num 2))
              (make-term 0 (num 2))))
            "Add dense-poly-x to sparse-poly-g")

;; 3. Subtraction tests
(assert-equ (sub sparse-poly-x dense-poly-zero)
            sparse-poly-x
            "Subtract sparse-poly-x from dense-poly-zero produces sparse-poly-x")

(assert-equ (sub dense-poly-x sparse-poly-empty)
            sparse-poly-x
            "Subtract dense-poly-x from sparse-poly-empty produces dense-poly-x")

(assert-equ (sub dense-poly-x sparse-poly-x)
            sparse-poly-empty
            "Subtract dense-poly-x from sparse-poly-x itself produces empty polynomial")

(assert-equ (sub dense-poly-x sparse-poly-w)
            (poly-sparse
             (list
              (make-term 2 (num 2))
              (make-term 1 (num 1))
              (make-term 0 (num 1))))
            "Subtract sparse-poly-w from dense-poly-x")


(assert-equ (sub sparse-poly-w dense-poly-x)
            (poly-sparse
             (list
              (make-term 2 (num -2))
              (make-term 1 (num -1))
              (make-term 0 (num -1))))
            "Subtract sparse-poly-w from dense-poly-x")

;; 4. Multiplication tests
(assert-equ (mul dense-poly-x sparse-poly-y)
            (poly-sparse
             (list
              (make-term 4 (num 12))
              (make-term 3 (num 17))
              (make-term 2 (num 16))
              (make-term 1 (num 7))
              (make-term 0 (num 2))))
            "Multiply dense-poly-x by sparse-poly-y")

(assert-equ (mul sparse-poly-x dense-poly-u)
            (poly-sparse
             (list
              (make-term 3 (num 12))
              (make-term 2 (num 17))
              (make-term 1 (num 10))
              (make-term 0 (num 3))))
            "Multiply sparse-poly-x by dense-poly-u")

(assert-equ (mul sparse-poly-x dense-poly-empty)
            sparse-poly-empty
            "Multiply sparse-poly-x by dense empty polynomial produces empty polynomial")

(assert-equ (mul dense-poly-x sparse-poly-zero)
            sparse-poly-empty
            "Multiply dense-poly-x by zero polynomial produces empty polynomial")


(define p1
  (make-polynomial 'x
                   (make-sparse-term-list
                    (list (make-term 2 (num 1))
                          (make-term 0 (num 1))))))

(define p2
  (make-polynomial 'x
                   (make-sparse-term-list
                    (list (make-term 3 (num 1))
                          (make-term 0 (num 1))))))


(define numerator
  (make-polynomial 'x
                   (make-sparse-term-list
                    (list (make-term 5 (num 2))
                          (make-term 3 (num 2))
                          (make-term 2 (num 2))
                          (make-term 0 (num 2))))))

(define denominator
  (make-polynomial 'x
                   (make-sparse-term-list
                    (list (make-term 4 (num 1))
                          (make-term 2 (num 2))
                          (make-term 0 (num 1))))))

(define rf (make-rational p2 p1))

(define rf-sum (make-rational numerator denominator))

(assert-equ (add rf rf)
            rf-sum
           "Add rational to rational does not reduce the fraction to the lowest terms")


(define p3
  (make-polynomial 'x
                   (make-sparse-term-list
                    (list (make-term 4 (num 1))
                          (make-term 3 (num -1))
                          (make-term 2 (num -2))
                          (make-term 1 (num 2))))))

(define p4
  (make-polynomial 'x
                   (make-sparse-term-list
                    (list (make-term 3 (num 1))
                          (make-term 1 (num -1))))))

(define gcd-of-p3-and-p4
  (make-polynomial 'x
                   (make-sparse-term-list
                    (list (make-term 2 (num -1))
                          (make-term 1 (num 1))))))


(assert-equ (greatest-common-divisor p3 p4)
            gcd-of-p3-and-p4
           "Greatest common divisor of two polynomials normalized")

(define P1
  (make-polynomial 'x
                  (make-sparse-term-list
                   (list (make-term 2 (num 1))
                         (make-term 1 (num -2))
                         (make-term 0 (num 1))))))
                         

(define P2
  (make-polynomial 'x
                  (make-sparse-term-list
                   (list (make-term 2 (num 11))
                         (make-term 0 (num 7))))))
                         

(define P3
  (make-polynomial 'x
                  (make-sparse-term-list
                   (list (make-term 1 (num 13))
                         (make-term 0 (num 5))))))

(define Q1 (mul P1 P2))
(define Q2 (mul P1 P3))


(assert-equ (greatest-common-divisor Q1 Q2)
            P1
           "Greatest common divisor of Q1 and Q2 = P1")

