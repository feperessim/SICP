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
  
  
