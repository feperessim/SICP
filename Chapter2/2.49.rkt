(define (outline-frame frame)
  ((segments->painter
    (list (make-segment (make-vec 0 0)
			(make-vec 0 1))
	  (make-segment (make-vec 0 1)
			(make-vec 1 1))
	  (make-segment (make-vec 1 1)
			(make-vec 1 0))
	  (make-segment (make-vec 1 0)
			(make-vec 0 0))))
   frame))

(define (paint-X frame)
  ((segments->painter
    (list (make-segment (make-vec 0 0)
			(make-vec 1 1))
	  (make-segment (make-vec 1 0)
			(make-vec 0 1))))
   frame))

(define (paint-diamond frame)
  ((segments->painter
    (list (make-segment (make-vec 0 .5)
			(make-vec .5 1))
	  (make-segment (make-vec .5 1)
			(make-vec 1 .5))
	  (make-segment (make-vec 1 .5)
			(make-vec .5 0))
	  (make-segment (make-vec .5 0)
			(make-vec 0 .5))))
   frame))

			
		     
  
  
  
