(define (rand-update x)
  (modulo (+ (* 1664525 x) 1013904223) 4294967296))

(define (make-random-generator seed)
  (define (generate)
    (set! seed (rand-update seed))
    seed)
  (define (reset new-seed)
    (set! seed new-seed))
  (define (dispatch m)
    (cond ((eq? m 'generate) generate)
          ((eq? m 'reset) reset)
          (else (error "Unknown request -- MAKE-RANDOM-GENERATOR"
                       m))))
  dispatch)

(define rand (make-random-generator 47))

((rand 'generate)) ;; => 1092136898
((rand 'generate)) ;; => 2326342713
((rand 'reset) 47)
((rand 'generate)) ;; => 1092136898
