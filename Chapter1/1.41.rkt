(define (double p)
  (lambda (x)
    (p
     (p x))))

;; test cases

((double inc) 1) ;; => 3

(((double (double double)) inc) 5) ;; => 21

