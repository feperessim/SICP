;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 1.12|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (pascal m n)
  (cond ((= m n) 1)
        ((= n 0) 1)
        (else (+ (pascal (- m 1) n)
                 (pascal (- m 1) (- n 1))))))