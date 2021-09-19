(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
	((= x (entry set)) set)
	((< x (entry set))
	 (make-tree (entry set)
		    (adjoin-set x (left-branch set))
		    (right-branch set)))
	((> x (entry set))
	 (make-tree (entry set)
		    (left-branch set)
		    (adjoin-set x (right-branch set))))))
	 
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list (right-branch tree)
					  result-list)))))
  (copy-to-list tree '()))
  

(define (list->tree elements)
  (car (partial-tree 
        elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size 
             (quotient (- n 1) 2)))
        (let ((left-result 
               (partial-tree 
                elts left-size)))
          (let ((left-tree 
                 (car left-result))
                (non-left-elts 
                 (cdr left-result))
                (right-size 
                 (- n (+ left-size 1))))
            (let ((this-entry 
                   (car non-left-elts))
                  (right-result 
                   (partial-tree 
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree 
                     (car right-result))
                    (remaining-elts 
                     (cdr right-result)))
                (cons (make-tree this-entry 
                                 left-tree 
                                 right-tree)
                      remaining-elts))))))))


(define (union-set set1 set2)
  (define (union-set-list set1 set2)
    (cond ((null? set1) set2)
	  ((null? set2) set1)
	  (else
	   (let ((x1 (car set1)) (x2 (car set2)))
	     (cond ((= x1 x2)
		    (cons x1 (union-set-list
			      (cdr set1)
			      (cdr set2))))
		   ((< x1 x2) (cons x1 (union-set-list
					(cdr set1)
					set2)))
		   (else (cons x2 (union-set-list
				   set1
				   (cdr set2)))))))))
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else
	 (list->tree
	  (union-set-list (tree->list-2 set1)
			  (tree->list-2 set2))))))
	

(define (intersection-set set1 set2)
  (define (intersection-set-list set1 set2)
    (if (or (null? set1) (null? set2))
	'()
	(let ((x1 (car set1)) (x2 (car set2)))
	  (cond ((= x1 x2)
		 (cons x1 (intersection-set-list
			   (cdr set1)
			   (cdr set2))))
		((< x1 x2) (intersection-set-list
			    (cdr set1) 
			    set2))
		((< x2 x1) (intersection-set-list
			    set1 
			    (cdr set2)))))))
  (if (or (null? set1) (null? set2))
      '()
      (list->tree
       (intersection-set-list (tree->list-2 set1)
			      (tree->list-2 set2)))))
  

;; (define (union-set set1 set2)
;;   (define (union-set-iter set1-list set2)
;;     (if (null? set1-list)
;; 	set2
;; 	(union-set-iter (cdr set1-list)
;; 			(adjoin-set (car set1-list) ;; n log n growth
;; 				    set2))))
;;   (cond ((null? set1) set2)
;; 	((null? set2) set1)
;; 	(else
;; 	 (union-set-iter (tree->list-2 set1)
;; 			 set2))))
	       

;; testcases

(define tree1
  (adjoin-set 11 (adjoin-set 9 (adjoin-set 1 (adjoin-set 5 (adjoin-set 3 (adjoin-set 7 '())))))))


(define tree2
  (adjoin-set 12 (adjoin-set 9 (adjoin-set 5 (adjoin-set 7 (adjoin-set 1 (adjoin-set 3 '())))))))

(define tree3
  (adjoin-set 5 (adjoin-set 8 (adjoin-set 0 (adjoin-set 111 (adjoin-set 3 (adjoin-set 77 '())))))))

(union-set tree1 tree2)

(union-set tree1 tree3)

(union-set tree2 tree3)

(union-set tree1 '())

(intersection-set tree1 tree2)

(intersection-set tree1 tree3)

(intersection-set tree2 tree3)
