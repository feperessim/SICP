(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) 
                        quarter)))
      (below (flip-vert half) half))))

(define smile
  (list
   (make-segment (make-vect 0.2 0.7) (make-vect 0.4 0.7)) ;; left eye
   (make-segment (make-vect 0.6 0.7) (make-vect 0.8 0.7)) ;; right eye
   (make-segment (make-vect 0.3 0.3) (make-vect 0.15 0.1)) ;; mouth
   (make-segment (make-vect 0.15 0.1) (make-vect 0.65 0.1)) ;; mouth
   (make-segment (make-vect 0.65 0.1) (make-vect 0.7 0.3)))) ;; mouth
 
(define (corner-split painter n)
  (if (= n 0)
      painter
      (beside (below painter (up-split painter (- n 1)))
	      (below (right-split painter (- n 1))
		     (corner-split painter (- n 1))))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four identity flip-horiz
                                  flip-vert rotate180)))
    (combine4 (corner-split painter n))))
