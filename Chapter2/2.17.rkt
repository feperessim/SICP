(define (last-pair l)
  (if (null? (cdr l))
      (list (car l))
      (last-pair (cdr l))))

;; test cases
(last-pair (list 23 72 149 34)) ;; 34
(last-pair (list 23)) ;; 23
(last-pair (list )) ;; error




      
      