(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1)
	       (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (append set1 set2))


;; There are no changes in element-of set and intersection-set;
;; However adjoin-set and union-set did change. adjoin-set now is O(1)
;; since it does not need to check whether an element is already in
;; the set.

;; union set is now O(n), where n is the size of list 1,
;; since it is now using append, it only needs to walk
;; through every element of list1, instead to
;; for every element in lsit 1 compare against every element
;; in list 2.

