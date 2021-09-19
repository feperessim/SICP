#lang racket

(define zero (λ (f) (λ (x) x)))
(define one (λ (f) (λ (x) (f x))))
(define two (λ (f) (λ (x) (f (f x)))))

(define (add-1 n)
  (λ (f) (λ (x) (f ((n f) x)))))

(define (+ a b)
  (λ (f) (λ (x) (((a f)(b f)) x))))

(add-1 zero)
(+ one two)

;; Substitutions model to(add-1 zero)
;; (λ (f) (λ (x) (f ((zero f) x))))
;; (λ (f) (λ (x) (f (((λ (f) (λ (x) x)) f) x))))
;; (λ (f) (λ (x) (f (((λ (x) x) x)))))
;; (λ (f) (λ (x) (f  x)))




         
