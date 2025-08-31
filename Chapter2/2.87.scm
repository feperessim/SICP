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

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types:
           APPLY-GENERIC"
           (list op type-tags))))))

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
  'done)

(define (install-polynomial-package)
  ;;internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1)
         (variable? v2)
         (eq? v1 v2)))
  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list)
    (null? term-list))
  (define (make-term order coeff)
    (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (=zero-poly? p)
    (if (empty-termlist? (term-list p))
        #t
        (and (=zero? (coeff
                      (first-term
                       (term-list p))))
             (=zero? (tag
                      (make-poly
                       (variable p)
                       (rest-terms
                        (term-list p))))))))
  
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

;; Constructor for scheme numbers
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;; test cases
(install-scheme-number-package)
(install-polynomial-package)

(define poly-x
  (make-polynomial 'x 
    (list (make-term 2 (make-scheme-number 3))
          (make-term 1 (make-scheme-number 2))
          (make-term 0 (make-scheme-number 1)))))

(define poly-y
  (make-polynomial 'x 
    (list (make-term 2 (make-scheme-number 3))
          (make-term 1 (make-scheme-number 2))
          (make-term 0 (make-scheme-number 1)))))

(define poly-w
  (make-polynomial 'x
    (list (make-term 3 (make-scheme-number 3))
          (make-term 1 (make-scheme-number 2))
          (make-term 0 (make-scheme-number 1)))))

(define poly-zero
  (make-polynomial 'x 
    (list (make-term 2 (make-scheme-number 0))
          (make-term 1 (make-scheme-number 0))
          (make-term 0 (make-scheme-number 0)))))

(define poly-empty
  (make-polynomial 'x '()))

(define poly-g
  (make-polynomial 'y
    (list (make-term 3 (make-scheme-number 3)))))

(define poly-k
  (make-polynomial 'x
    (list (make-term 3 poly-g)
          (make-term 1 (make-scheme-number 2))
          (make-term 0 (make-scheme-number 1)))))

(define poly-h
  (make-polynomial 'x
    (list (make-term 3 poly-g))))

(display (add poly-x poly-empty))
(newline)
(display (add poly-x poly-zero))
(newline)
(display (add poly-x poly-y))
(newline)
(display (add poly-x poly-w))
(newline)
(display (add poly-k poly-h))
(newline)
(display (mul poly-x poly-empty))
(newline)
(display (mul poly-x poly-zero))
(newline)
(display (mul poly-x poly-y))
(newline)
(display (mul poly-x poly-w))
(newline)
