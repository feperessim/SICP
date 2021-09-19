;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader{lib "htdp-advanced-reader.ss" "lang"}{{modname |Exercise 1.3|} {read-case-sensitive #t} {teachpacks ()} {htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)}}
;; Exercise 1.3: Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.

(define (square x) (* x x))

(define (sum-of-squares a b)
  (+ (square a) (square b)))

(define (smallest a b)
  (if (< a b) a b))

(define (smallest-of-three a b c)
  (smallest (smallest a b) c))

(define (sum-of-squares-of-two-largest a b c)
  (cond ((equal? a (smallest-of-three a b c)) (sum-of-squares b c))
        ((equal? b (smallest-of-three a b c)) (sum-of-squares a c))
        (else (sum-of-squares a b))))
      
