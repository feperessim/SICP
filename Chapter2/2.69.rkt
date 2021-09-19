(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
	'()
	(let ((next-branch
	       (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch)
		    (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else (error "bad bit -- CHOOSE-BRANCH" bit))))


(define (encode message tree)
  (if (null? message)
      '()
      (append
       (encode-symbol (car message)
		      tree)
       (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree)
	 (if (eq? (symbol-leaf tree) symbol)
	     '()
	     (error "Unknown symbol" symbol)))
	(else
	 (let ((left (left-branch tree))
	       (right (right-branch tree)))
	   (if (memq symbol (symbols left))
	       (cons 0 (encode-symbol symbol left))
	       (cons 1 (encode-symbol symbol right)))))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set)))
	 (cons x set))
	(else
	 (cons (car set)
	       (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set
	 (make-leaf (car pair)   ;symbol
		    (cadr pair)) ;frequency
	 (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge 
   (make-leaf-set pairs)))

;; (define (successive-merge list-of-pairs)
;;   (if (null? (cdr list-of-pairs))
;; 	(car list-of-pairs)
;; 	(make-code-tree (car list-of-pairs)
;; 			(successive-merge (cdr list-of-pairs)))))

(define (successive-merge list-of-pairs)
  (if (null? (cdr list-of-pairs))
      (car list-of-pairs)
      (successive-merge
       (adjoin-set (make-code-tree (car list-of-pairs)
				   (cadr list-of-pairs))
		   (cddr list-of-pairs)))))

(define sample-tree
  (make-code-tree
   (make-leaf 'D 1)
   (make-code-tree
    (make-leaf 'C 1)
    (make-code-tree
     (make-leaf 'B 2)
     (make-leaf 'A 4)))))

(define pairs '((A 4) (B 2) (C 1) (D 1)))

(generate-huffman-tree pairs)

;; result
;;  ((leaf D 1) ((leaf C 1) ((leaf B 2) (leaf A 4) (B A) 6) (C B A) 7) (D C B A) 8

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message (generate-huffman-tree pairs))

(encode (decode sample-message sample-tree) (generate-huffman-tree pairs))

;; result
;; '(0 1 1 0 0 1 0 1 0 1 1 1 0)
