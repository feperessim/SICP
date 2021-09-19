(define (same-parity x . numbers)
  (let ((x-remainder (remainder x 2)))
    (define (make-list-same-parity num-list)
      (cond ((null? num-list) nil)
            ((= x-remainder
                (remainder (car num-list) 2))
             (cons (car num-list)
                   (make-list-same-parity (cdr num-list))))
        (else (make-list-same-parity (cdr num-list)))))
    (cons x (make-list-same-parity numbers))))

;; test cases
(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
