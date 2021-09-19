(require racket/include)
(include "2.46.rkt")
(include "2.47.rkt")

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
	((frame-coord-map frame) (start-segment segment))
	((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define (make-segment v w)
  (cons v w))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))
