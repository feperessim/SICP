;; recursive cont-frac process

(define (cont-frac n d k)
  (define (try i)
    (if (= k i)
        i
        (/ (n i)
             (+ (d i)
                (try (inc i))))))
  (try 1))

(define (close-enough? x y)
  (< (abs (- x y)) 0.0001))

;; test procedure for k size
(define (try-cont-frac guess)
  (if (close-enough? (cont-frac (lambda (i) 1.0)
                               (lambda (i) 1.0)
                               guess)
                    (cont-frac (lambda (i) 1.0)
                               (lambda (i) 1.0)
                               (+ 1 guess)))
      guess
      (try-cont-frac (+ guess 1))))

(define phi (/ (+ 1 (sqrt 5)) 2))

(define (inc n) (+ n 1))

(< (abs (- (cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           (try-cont-frac 1))
(/ 1 phi))) 0.0001)

;; K must be at least 12, obtain an approximation of at least four decimal places 

;; Iterative cont-frac process

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= k i)
        result
        (iter (inc i)
              (/ (n i)
             (+ result
                (d i))))))
  (iter 1 1))

(try-cont-frac 1)

(cont-frac-iter (lambda (i) 1.0)
                (lambda (i) 1.0)
                (try-cont-frac 1))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           (try-cont-frac 12))
