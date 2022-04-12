(define (new-if predicate 
                then-clause 
                else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (square x)
  ( * x x))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ ( + x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

;; If the interpreter is using the applicative order of evaluation
;; The program will enter in an infintity loop when the boddy of the
;; procedure be replaced with the operands, because (sqrt-iter 
;; will be called always called, even when the guess be the root
;; the formal parameters of new-if will call the procedure again, and over again
;; It might even lead to errors with the normal order of evaluation
