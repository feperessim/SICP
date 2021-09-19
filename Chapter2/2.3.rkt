#lang sicp

(define (make-segment p1 p2)
  (cons (start-segment (car p1) (cdr p1))
        (end-segment (car p2) (cdr p2))))

(define (start-segment x y)
  (make-point x y))

(define (end-segment x y)
  (make-point x y))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (mid-point line-segment)
  (cons (average (x-point (car line-segment))
                 (x-point (cdr line-segment)))
        (average (y-point (car line-segment))
                 (y-point (cdr line-segment)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (average x y)
  (/ (+ x y) 2.0))


(define (make-rectangle horizontal-segment vertical-segment)
  (cons horizontal-segment vertical-segment))

(define (horizontal-segment p1 p2)
  (make-segment p1 p2))

(define (vertical-segment p1 p2)
  (make-segment p1 p2))

(define (perimeter rectangle)
  (+ (* 2 (length (car rectangle)))
     (* 2 (length (cdr rectangle)))))


(define (area rectangle)
  (* (length (car rectangle))
     (length (cdr rectangle))))
                     
(define (length segment)
    (sqrt (+ (square (- (x-point (car segment))
                        (x-point (cdr segment))))
             (square (- (y-point (car segment))
                        (y-point (cdr segment)))))))

(define rectangle (make-rectangle
                   (horizontal-segment (make-point 3 2)
                                       (make-point 7 2))
                   (vertical-segment (make-point 3 5)
                                     (make-point 7 5))))
(define (square x) (* x x))

(perimeter rectangle)
(area rectangle)


;; another representation of make-rectangle
(define (make-rectangle left-bottom-corner-point right-up-corner-point)
  (cons (horizontal-segment left-bottom-corner-point
                             (right-bottom-corner-point left-bottom-corner-point right-up-corner-point))
        (vertical-segment (left-up-corner-point left-bottom-corner-point right-up-corner-point)
                           right-up-corner-point)))                             
                      
(define (right-bottom-corner-point left-bottom-corner-point right-up-corner-point)
  (make-point (x-point right-up-corner-point)
              (y-point left-bottom-corner-point)))

(define (left-up-corner-point left-bottom-corner-point right-up-corner-point)
  (make-point (x-point left-bottom-corner-point)
              (y-point right-up-corner-point)))

(define rectangle (make-rectangle
                   (make-point 3 2)
                   (make-point 7 5)))

(perimeter rectangle)
(area rectangle)
