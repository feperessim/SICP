#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; test case for accumulate
;; outputs 55 and 720
(accumulate + 0 (list 1 2 3 4 5 6 7 8 9 10))
(accumulate * 1 (list 2 3 4 5 6))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

;; test case for map
;; the square of numbers 1 through 5
;; output (mcons 1 (mcons 4 (mcons 9 (mcons 16 (mcons 25 '())))))
(map (lambda (x) (* x x)) (list 1 2 3 4 5))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

;; test case for append
;; (display (append (list 10 9 8)  (list 1 2 3))
;; (10 9 8 1 2 3)


(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;; test case to length
;; (length (cons (list 1 2) (list 3 4)))
;; 3