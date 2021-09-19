(define (deep-reverse l)
  (define (deep-reverse-iter l item)
    (cond ((null? l) item)
          ((pair? (car l))
           (deep-reverse-iter (cdr l)
                              (cons (deep-reverse-iter (car l) nil) item)))
          (else (deep-reverse-iter (cdr l) (cons (car l) item)))))
  (deep-reverse-iter l nil))

;; test case

(define x (list (list 1 2) (list 3 4))) ;; prints ((4 3) (2 1))
(display (deep-reverse x))
(newline)


