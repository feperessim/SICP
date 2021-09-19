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

(define pairs '((a 2) (boom 1) (Get 2) (job 2) (na 16) (Sha 3) (yip 9) (Wah 1)))

(define sample-tree (generate-huffman-tree pairs))

(define sample-message
  '(Get a job Sha na na na na na na na na
	Get a job Sha na na na na na na na na
	Wah yip yip yip yip
	yip yip yip yip yip
	Sha boom))
			     

(encode sample-message sample-tree)

;; result
;;  (1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0
;;   0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0
;;   0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0
;;   1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)

;; There are 84 bits required for encoding

;; USinh fixed-length, since we have 8 symbols, then we would need
;; 3 bits per symbol to encode the song. Thus we would need 3 * 36
;; (quanity of bits) x (quantity of symbols) = 108 bits.
