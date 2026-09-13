(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
    (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (let ((experiment
         (lambda () (P (random-in-range x1 x2)
                       (random-in-range y1 y2)))))
  (monte-carlo trials experiment)))

(define (estimate-pi P x1 x2 y1 y2 trials)
  (*
   (* (- x2 x1)
      (- y2 y1))
   (estimate-integral P x1 x2 y1 y2 trials)))

(define (square x)
  (* x x))

(define (predicate x y)
  (<= (+ (square x) (square y)) 1))

(map (lambda (trials)
       (exact->inexact
        (estimate-pi predicate -1.0 1.0 -1.0 1.0 trials)))
     (list 1 10 100 1000 10000 100000 1000000))

;; => (4.0 2.4 3.24 3.148 3.168 3.14836 3.1433)
