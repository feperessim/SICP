(define (make-monitored f)
  (define counter 0)
  (define (mf input)
    (if (symbol? input)
        (cond ((eq? input 'how-many-calls?)
               counter)
              ((eq? input 'reset-count)
               (begin (set! counter 0) #t))
              (else (error "Unknown request")))
        (begin (set! counter (+ counter 1))
               (f input))))
  mf)

(define monitored-sqrt (make-monitored sqrt))

(monitored-sqrt 2) ;; => 1.4142135623730951
(monitored-sqrt 2) ;; => 1.4142135623730951
(monitored-sqrt 2) ;; => 1.4142135623730951w
(monitored-sqrt 'how-many-calls?) ;; => 3
(monitored-sqrt 'reset-count) ;; => #t
(monitored-sqrt 'how-many-calls?) ;; => 0
