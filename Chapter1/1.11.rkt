;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 1.11|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; f(n) = n if n < 3
;; f(n) = f(n - 1) + 2*f(n - 2)+ f(n - 3)

(define (f-recursive n)
  (if (< n 3)
      n
      (+ (f-recursive (- n 1))
            (* 2 (f-recursive (- n 2)))
            (* 3 (f-recursive (- n 3))))))

(define (f-iter a b c n)
  (if (< n 3)
      a
      (f-iter (+ a  b b  c c c) a b (- n 1))))

(define (f n)
  (f-iter 2 1 0 n))
  