(define (fringe l)
  (cond ((null? l) nil)
        ((not (pair? l)) (cons l nil))
        (else (append (fringe (car l))
              (fringe (cdr l))))))
                         

(define x (list (list 1 2) (list 3 4)))
;;(fringe x) (1 2 3 4)
;;(fringe (list x x) (1 2 3 4 1 2 3 4)
(display (fringe x)) 
(display (fringe (list x x)))
