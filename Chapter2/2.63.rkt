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
	 
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
	      (cons (entry tree)
		    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list (right-branch tree)
					  result-list)))))
  (copy-to-list tree '()))
  

(define tree1
  (adjoin-set 11 (adjoin-set 9 (adjoin-set 1 (adjoin-set 5 (adjoin-set 3 (adjoin-set 7 '())))))))


(define tree2
  (adjoin-set 11 (adjoin-set 9 (adjoin-set 5 (adjoin-set 7 (adjoin-set 1 (adjoin-set 3 '())))))))

(define tree3
  (adjoin-set 11 (adjoin-set 7 (adjoin-set 9 (adjoin-set 1 (adjoin-set 3 (adjoin-set 5 '())))))))

;; (a)

;; The procedures always produce the same result for every tree
;; For the example 2.16 they produce the following list: '(1 3 5 7 9 11)

;; (b)

;; The order of growth is different.
;; tree->list-1 grows slowly since it uses append to combine every
;; resulting list
;; tree->list-1 - O(n log n) growth when balanced - O(n^2) worst case
;; tree->list-2 - O(n) growth
