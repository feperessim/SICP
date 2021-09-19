(define (square-list items)
  (if (null? items)
      nil
      (cons ((lambda (x) (* x x)) (car items))
            (square-list (cdr items)))))

;; test case
(square-list (list 1 2 3 4 5))

(define (square-list items)
  (map (lambda (x) (* x x)) items))

;; test case
(square-list (list 1 2 3 4 5))