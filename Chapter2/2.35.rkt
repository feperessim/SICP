#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumulate + 0  (map
                    (lambda (x) (if (not (pair? x)) 1 (count-leaves (car t))))
                        t)))

;; test case
;; output 6
(define x (list (list 1 2 3) (list 4 5 6)))
(count-leaves x)