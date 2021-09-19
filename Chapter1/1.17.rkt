;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 1.17|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define ( double a)
  (* 2 a))

(define (halve a)
  (/ a 2))

(define (mult a b)
  (cond ((= b 0) b)
        ((even? b) (double (mult a (halve b))))
        (else (+ a ( mult a (- b 1)))))) 