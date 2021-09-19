#lang sicp

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (append (list (car s)) x)) rest)))))

(subsets (list 1 2 3))

;; Explanation of why it works.
;; When the recursion ends, the list would have reached the end,
;; at this point '() will be returned.
;; s will have a single element which is 3, so '()  and three
;; combined into list, thus we have ((), (3)), and it is returned.
;; Now car s has the element 2, and rest is a list, then 2 is combined which each
;; element of this list thus we have ((), (3), (2), (3 2)) which is returned.
;; Now the last run, car s has 1, which is combine with each element of rest
;; creating the final list ((), (3), (2), (3 2) (1) (1 3) (1 2) (1 2 3)) which is
;; the set of all subsets of the original list.