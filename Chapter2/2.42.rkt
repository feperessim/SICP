(define nil '())
(define empty-board nil)

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc sequence)
  (accumulate append nil (map proc sequence)))

(define (queens board-size)
  (define (queens-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (λ (positions) (safe? k positions))
	 (flatmap
	  (λ (rest-of-queens)
	    (map (λ (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queens-cols (- k 1))))))
  (queens-cols board-size))

(define (adjoin-position new-row k rest-of-queens)
  (cons (cons new-row k) rest-of-queens))


(define (safe? k positions)
  (define (safe-pos? kth-queen positions)
    (if (null? positions)
	#t
	 (and
	  (not
	   (same-row-col?
	    kth-queen
	    (car positions)))
	  (not
	   (same-diagonal?
	    kth-queen
	    (car positions)))
	  (safe-pos? kth-queen (cdr positions)))))
  (let ((kth-queen (car positions)))
    (safe-pos? kth-queen (cdr positions))))

;; verifying whether the cols are equal is not necessary
(define (same-row-col? p1 p2)
  (or
   (=
    (car p1)
    (car p2))
   (=
    (cdr p1)
    (cdr p2))))

(define (same-diagonal? p1 p2)
  (let ((dx
	 (abs
	  (-
	   (car p1)
	   (car p2))))
	(dy
	 (abs
	  (-
	   (cdr p1)
	   (cdr p2)))))
    (= dx dy)))
   
	   
(queens 8)
