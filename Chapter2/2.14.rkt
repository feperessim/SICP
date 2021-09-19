(define (make-interval a b) (cons a b))

(define (lower-bound z)
  (car z))

(define (upper-bound z)
  (cdr z))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
  (make-interval (min p1 p2 p3 p4)
                 (max p1 p2 p3 p4))))

(define (div-interval x y)
  (cond ((<= (* (lower-bound y)
                (upper-bound y)) 0)
      (display "The interval [")
      (display (lower-bound y))
      (display ", ")
      (display (upper-bound y))
      (display"], spans zero")
      (newline))
  (else (mul-interval x
                      (make-interval (/ 1.0 (upper-bound y))
                                     (/ 1.0 (lower-bound y)))))))

(define (make-center-percent c p)
  (let ((percentage-tolerance (* c (/ p 100.0))))
  (make-interval (- c percentage-tolerance) (+ c percentage-tolerance))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percentage-tolerance i)
    (* (/ (width i) (center i)) 100))


(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define A (make-interval 25 50))
(define B (make-interval 50 75))

(div-interval A A)
(div-interval A B)
(newline)
(par1 A B)
(par2 A B)
(newline)

(define A (make-center-percent 25 10))
(define B (make-center-percent 50 10))
(par1 A B)
(par2 A B)
