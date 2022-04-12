(define (cube x) (* x x x))

(define (p x) (- ( * 3 x) (* 4 ( cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;; a)
;; (sin 12.15)
;; (p (sin 4.05))
;; (p (p (sin 1.35)))
;; (p (p (p (sin 0.45))))
;; (p (p (p (p (sin 0.15)))))
;; (p (p (p (p (p (sin 0.05))))))

;; p applies five times.

;; b) Order of Growth in space is O(n/2) which is O(n)

;; Order of Growth in time is O(log n)
