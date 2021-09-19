(define (rotate180 painter)
  (rotate90 (rotate90 painter)))

(define (rotate270 painter)
  (rotate90 (rotate180 painter)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
	(let ((paint-left (transform-painter
			   painter1
			   (make-vect 0.0 0.0)
			   split-point
			   (make-vect 0.0 1.0)))
	      (paint-right (transform-painter
			    painter2
			    split-point
			    (make-vect 1.0 0.0)
			    (make-vect 0.5 1.0))))
	  (lambda (frame)
	    (paint-left frame)
	    (paint-right frame)))))


(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
	(let ((paint-bottom (transform-painter
			   painter1
			   (make-vect 0.0 0.0)
			   split-point
			   (make-vect 1.0 0.0)))
	      (paint-top (transform-painter
			    painter2
			    split-point
			    (make-vect 1.0 0.5)
			    (make-vect 0.0 1.0))))
	  (lambda (frame)
	    (paint-bottom frame)
	    (paint-top frame)))))


(define (bellow2 painter1 painter2)
  (let ((p1-rotated90 (rotate90 painter1))
	(p2-rotated90 (rotate90 painter2)))
    (rotate270
     (beside p1-rotated90
	     p1-rotated90))))
