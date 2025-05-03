;; Data directed programming - Tagged data
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: 
                    TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: 
                    CONTENTS" datum)))

;;; Data directed programming - Operation Table
(define operation-table (make-hash))

(define (put op types proc)
  (hash-set! operation-table (cons op types) proc))

(define (get op types)
  (hash-ref operation-table (cons op types) #f))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) 
    (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) 
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

;; Complex
(define (install-complex-package)
  ;; imported procedures from rectangular 
  ;; and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 
          'rectangular) 
     x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) 
     r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag 
     (+ (real-part z1) (real-part z2))
     (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag 
     (- (real-part z1) (real-part z2))
     (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang 
     (* (magnitude z1) (magnitude z2))
     (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang 
     (/ (magnitude z1) (magnitude z2))
     (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) 
         (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) 
         (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) 
         (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) 
         (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))


(define (install-real-package)
  ;; internal procedures  
  (define (make-real n) n)
  
  (define (add-real x y)
    (make-real (+  x y)))
  (define (sub-real x y)
    (make-real (-  x  y)))
  (define (mul-real x y)
    (make-real (*  x  y)))
  (define (div-real x y)
    (make-real (/  x  y)))  
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (add-real x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (sub-real x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (mul-real x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (div-real x y))))
  (put 'make 'real
       (lambda (n) (tag (make-real n))))
  'done)

(define (make-real n)
  ((get 'make 'real) n))

;; Rational
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
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

;; Integer
(define (install-integer-package)
  ;; internal procedures  
  (define (make-integer n) n)
  
  (define (add-integer x y)
    (make-integer (+  x y)))
  (define (sub-integer x y)
    (make-integer (-  x  y)))
  (define (mul-integer x y)
    (make-integer (*  x  y)))
  (define (div-integer x y)
    (make-integer (/  x  y)))  
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'integer x))
  (put 'add '(integer integer)
       (lambda (x y) (tag (add-integer x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (sub-integer x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (mul-integer x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (div-integer x y))))
  (put 'make 'integer
       (lambda (n) (tag (make-integer n))))
  'done)

(define (make-integer n)
  ((get 'make 'integer) n))

;; Raise Ex 2.83
(define (raise arg)
  (apply-generic 'raise arg))

;;; Type table
(define type-level-table	 (make-hash))

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

;; Coercion procedurer - successive raise
(define (coerce arg type)
  (if (eq? (type-tag arg) type)
      arg
      (let ((proc (get 'raise (type-tag arg))))
	(if proc
	    (coerce (proc (contents arg)) type)
	    #f))))


(define (numer x) (car x))
(define (denom x) (cdr x))

;; Put raise procs in the table
(put 'raise 'integer
     (lambda (arg) (make-rational arg 1)))

(put 'raise 'rational
     (lambda (arg) (make-real (/ (numer arg) (denom arg)))))

(put 'raise 'real
     (lambda (arg) (make-complex-from-real-imag arg 0)))

;; Install numbers packages
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-real-package)
(install-rational-package)
(install-integer-package)

;; construct tower
(put-type-level 'integer 1)
(put-type-level 'rational 2)
(put-type-level 'real 3)
(put-type-level 'complex 4)


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((highest-type-tag (highest-type type-tags)))
            (let ((coerced-args 
                   (map (lambda (obj) 
                          (coerce obj highest-type-tag))
                        args)))
              (if (andmap identity coerced-args)
                  (apply apply-generic 
                         (cons op coerced-args))
                  (error "Could not coerce args" 
                         (list op type-tags)))))))))


;; Numbers
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

;; test cases
(define int (make-integer 3))
(define rat (make-rational 3 2))
(define real (make-real 2.5))
(define complex  (make-complex-from-real-imag 2 1))

(display (add int int))
(newline)
(display (add int rat))
(newline)
(display (add int real))
(newline)
(display (add int complex))
(newline)
(display (add rat rat))
(newline)
(display (add rat real))
(newline)
(display (add rat complex))
(newline)
(display (add real real))
(newline)
(display (add real complex))
(newline)
(display (add complex complex))
(newline)

;; => #<void>

;; (integer . 6)
;; (rational 9 . 2)
;; (real . 5.5)
;; (complex rectangular 5 . 1)
;; (rational 3 . 1)
;; (real . 4.0)
;; (complex rectangular 7/2 . 1)
;; (real . 5.0)
;; (complex rectangular 4.5 . 1)
;; (complex rectangular 4 . 2)
