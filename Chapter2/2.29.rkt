#lang sicp

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))


(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (total-weight mobile)
  (cond ((null? mobile) 0)
        ((not (pair? mobile)) mobile)
        (else (+ (total-weight
                  (branch-structure
                   (left-branch mobile)))
                 (total-weight
                  (branch-structure
                   (right-branch mobile)))))))


(define (balanced? mobile)
  (cond ((null? mobile) #t)
        ((not (pair? mobile)) mobile)
        (else
         (equal?
          (* (branch-length
              (left-branch mobile))
            (total-weight
             (branch-structure (left-branch mobile))))
          (* (branch-length
              (right-branch mobile))
            (total-weight
             (branch-structure (right-branch mobile))))))))


;; With cons representation only the selectors shall to be changed

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))
