(define (flip-horiz painter)
  (transform-painter
   painter
   (make-vect 1.0 0.0) ;new origin
   (make-vect 0.0 0.0) ;new end of edge 1
   (make-vect 1.0 1.0))) ;new end of edge 2

(define (rotate180 painter)
  (rotate90 (rotate90 painter)))

(define (rotate270 painter)
  (rotate90 (rotate180 painter)))


		     
