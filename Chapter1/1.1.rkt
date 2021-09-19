;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 1.1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; Exercise 1.1: Below is a sequence of expressions. What is the result printed by
;; the interpreter in response to each expression?
;; Assume that the sequence is to be evaluated in the order in which it is presented.

;; 10
;; (+ 5 3 4)
;; (- 9 1)
;; (/ 6 2)
;; (+ (* 2 4) (- 4 6))
;; (define a 3)
;; (define b (+ a 1))
;; (+ a b (* a b))
;; (= a b)
;; (if (and (> b a) (< b (* a b)))
;;    b
;;    a)
;; (cond ((= a 4) 6)
;;      ((= b 4) (+ 6 7 a))
;;      (else 25))
;; (+ 2 (if (> b a) b a))
;; (* (cond ((> a b) a)
;;         ((< a b) b)
;;         (else -1))
;;   (+ a 1))

10
12
8
3
19
#false
4
16
6
16
(define a 3)
(define b (+ a 1))

(+ 2 (if (> b a) b a))
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))