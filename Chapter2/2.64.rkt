(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

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

;; 1
;; partial-tree divides the list into tree parts:
;; left-tree, this-entry and right-tree
;; this-entry is the central tree, the left-tree
;; is the tree with items smaller than this-entry
;; and right-tree is the tree with items
;; greater than this-entry.

;;       5
;;     /  \  
;;    1   9
;;    \  / \
;;    3 7  11


;; 2
;; Since it has to access every element
;; in the list, we can easily see that
;; the order of growth is O(n)
