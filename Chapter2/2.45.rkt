(#%require sicp-pict)
;; (paint einstein)
(define wave einstein)

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter 
                                (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right 
                                   right))
              (corner (corner-split painter 
                                    (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right 
                         corner))))))

(define (flipped-pairs painter)
  (let ((painter2
	 (beside painter
		 (flip-vert painter))))
    (below painter2 painter2)))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) 
                        quarter)))
      (below (flip-vert half) half))))

(define wave4 (flipped-pairs wave))

(define wave5 (square-limit wave 1))


(define (square-of-four tl tr bl br)
  (λ (painter)
    (let ((top (beside (tl painter)
		       (tr painter)))
	  (bottom (beside (bl painter)
			  (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4
	 (square-of-four identity
			 flip-vert
			 identity
			 flip-vert)))
    (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4
	 (square-of-four flip-horiz
			 identity
			 rotate180
			 flip-vert)))
    (combine4 (coner-split painter n))))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter 
                                  (- n 1))))
        (beside painter 
                (below smaller smaller+)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter
			       (- n 1))))
	(below painter
	       (beside smaller smaller)))))

(define (split op-a op-b)
  (define (split-loop painter n)
    (if (= n 0)
	painter
	(let ((smaller (split-loop painter
				   (- n 1))))
	  (op-a painter
		(op-b smaller smaller)))))
  split-loop)

(define right-split (split beside below))
(define up-split (split below beside))
