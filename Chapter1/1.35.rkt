;; if x maps to 1 + 1/x it implies in x^2 = x + 1
;; thus let x to be the golden ratio denoted by the greek letter phi.
;; if phi maps to 1 + 1/phi then phi^2  = phi + 1
;; as far as we know that phi satisfies the equation phi2^= phi + 1
;; then phi is a fixed point of the transformation x maps to 1 + 1/x QED;

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define phi (/ (+ 1 (sqrt 5)) 2))

(fixed-point (lambda (y) (+ 1 (/ 1 y))) 1.0)

;; test case result
;; 1.6180327868852458