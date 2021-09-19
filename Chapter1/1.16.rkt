;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 1.16|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (square x)
  (* x x))

(define (exptt b n)
  (expt-iter-fast 1 b n))

(define (expt-iter-fast a b n)
  (cond ((= n 0) a)
        ((even? n) (expt-iter-fast a (square b) (/ n 2)))
        (else (expt-iter-fast (* a b) b (- n 1)))))
