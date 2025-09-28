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
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
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

(define (install-polynomial-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1)
         (variable? v2)
         (eq? v1 v2)))
  (define (make-term-list-with-zeros coeff order current-length)
    (let ((size (- order current-length)))
      (if (= size 0)
          (list coeff)
          (cons coeff
                (make-term-list-with-zeros
                 (make-scheme-number 0)
                 order
                 (+ current-length 1))))))
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
  (define (the-empty-termlist) '())
  (define (first-term term-list)
    (make-term (- (length term-list) 1)
               (car term-list)))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list)
    (null? term-list))
  (define (make-term order coeff)
    (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
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
        (the-empty-termlist)
        (add-terms 
         (mul-term-by-all-terms 
          (first-term L1) L2)
         (mul-terms (rest-terms L1) L2))))  
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term 
            (+ (order t1) (order t2))
            (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms 
            t1 
            (rest-terms L))))))
  (define (negate-poly p)
    (make-poly (variable p)
               (negate-terms (term-list p))))
  (define (negate-terms L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t (first-term L)))
          (adjoin-term
           (make-term
            (order t)
            (negate (coeff t)))
           (negate-terms (rest-terms L))))))
  (define (sub-poly p1 p2)
    (tag (add-poly p1 (negate-poly p2))))
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
  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (make-term order coeff)
  ((get 'make-term 'polynomial) order coeff))

;; Defining generic operations
(define (=zero? z)
  (apply-generic '=zero? z))
(define (add x y) (apply-generic 'add x y))
(define (mul x y) (apply-generic 'mul x y))
(define (sub x y) (apply-generic 'sub x y))
(define (negate x) (apply-generic 'negate x))

;; Constructor for scheme numbers
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;; test cases
(install-scheme-number-package)
(install-polynomial-package)

;; Set up the tower levels
(put-type-level 'scheme-number 1)
(put-type-level 'polynomial 2)

;; Install raise operations
;; This is problematic for when the polynomial is in a different variable
(put 'raise '(scheme-number)
     (lambda (arg) (make-polynomial 'x (list (make-scheme-number arg)))))

;; Install project operations
(put 'project '(polynomial)
     (lambda (z) (make-scheme-number (car z))))

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

;; Convenience constructors
(define (num n) (make-scheme-number n))
(define (poly terms) (make-polynomial 'x terms))

(define poly-x
  (make-polynomial 'x
                   (list (make-scheme-number 3)
                         (make-scheme-number 2)
                         (make-scheme-number 1))))
(define poly-y
  (make-polynomial 'x
                   (list (make-scheme-number 4)
                         (make-scheme-number 3)
                         (make-scheme-number 2))))
(define poly-u
  (make-polynomial 'x
                   (list (make-scheme-number 4)
                         (make-scheme-number 3))))

(define poly-w
  (make-polynomial 'x
                   (list (make-scheme-number 1)
                         (make-scheme-number 1)
                         (make-scheme-number 0))))

(define poly-zero
  (make-polynomial 'x
                   (list (make-scheme-number 0)
                         (make-scheme-number 0)
                         (make-scheme-number 0))))
(define poly-g
  (make-polynomial 'x
                   (list (make-scheme-number 4)
                         (make-scheme-number 3)
                         (make-scheme-number 0)
                         (make-scheme-number 1))))

(define poly-empty (make-polynomial 'x '()))


;; 1. Negation tests
(assert-equ (negate (num 2))
            (num -2)
            "Negate scheme-number")

(assert-equ (negate poly-y) 
            (poly (list (num -4) (num -3) (num -2)))
            "Negate poly-y")

(assert-equ (negate poly-zero)
            poly-empty
            "Negate poly-zero produces empty polynomial")

;; 2. Addition tests
(assert-equ (add poly-x poly-zero)
            poly-x
            "Add poly-x to poly-zero produces poly-x")

(assert-equ (add poly-x poly-empty)
            poly-x
            "Add poly-x to poly-empty produces poly-x")

(assert-equ (add poly-x poly-x)
            (poly (list (num 6) (num 4) (num 2)))
            "Add poly-x to itself produces 2 timex poly-x")

(assert-equ (add poly-x poly-w)
            (poly (list (num 4) (num 3) (num 1)))
            "Add poly-x to poly-w")

(assert-equ (add poly-x poly-g)
            (poly (list (num 4) (num 6) (num 2) (num 2)))
            "Add poly-x to poly-g")

;; 3. Subtraction tests
(assert-equ (sub poly-x poly-zero)
            poly-x
            "Subtract poly-x from poly-zero produces poly-x")

(assert-equ (sub poly-x poly-empty)
            poly-x
            "Subtract poly-x from poly-empty produces poly-x")

(assert-equ (sub poly-x poly-x)
            poly-empty
            "Subtract poly-x from itself produces empty polynomial")

(assert-equ (sub poly-x poly-w)
            (poly (list (num 2) (num 1) (num 1)))
            "Subtract poly-w from poly-x")

(assert-equ (sub poly-w poly-x)
            (poly (list (num -2) (num -1) (num -1)))
            "Subtract poly-x from poly-w")


;; 4. Multiplication tests
(assert-equ (mul poly-x poly-y)
            (poly (list (num 12) (num 17) (num 16) (num 7) (num 2)))
            "Multiply poly-x by poly-y")

(assert-equ (mul poly-x poly-u)
            (poly (list (num 12) (num 17) (num 10) (num 3)))
            "Multiply poly-x by poly-u")

(assert-equ (mul poly-x poly-empty)
            poly-empty
            "Multiply poly-x by empty polynomial produces empty polynomial")

(assert-equ (mul poly-x poly-zero)
            poly-empty
            "Multiply poly-x by zero polynomial produces empty polynomial")


;; 5. =zero? tests
(assert-equal (=zero? poly-empty) #t "poly-empty is zero")
(assert-equal (=zero? poly-x) #f "poly-x is not zero")
(assert-equal (=zero? (negate poly-zero)) #t "negate poly-zero is zero")
