;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 1.18|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define ( double a)
  (* 2 a))

(define (halve a)
  (/ a 2))

(define (iter-mult a b)
  (fast-iter-mult 0 a b))

(define (fast-iter-mult k a b)
  (cond ((= b 0) k)
        ((even? b) (fast-iter-mult k (double a) (halve b)))
        (else (fast-iter-mult (+ k a) a (- b 1)))))