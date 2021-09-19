(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))
  
(define (reverse l)
  (let ((len (length l)))
    (define (reverse-iter l len)
      (if (= len 0)
      nil
      (cons (list-ref l (- len 1))
            (reverse-iter l (- len 1)))))
  (reverse-iter l len)))

(define (reverse l)
  (if (null? (cdr l))
      (list (car l))
      (append
       (reverse
	(cdr l)) (list (car l)))))
		 
;; test cases
(display (reverse (list 1 2 3 4 5)))
(newline)
(display (reverse (list 1)))
(newline)
(display (reverse (list 1 2)))

