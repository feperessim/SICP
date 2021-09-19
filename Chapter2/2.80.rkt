(define (=zero? z)
  (apply-generic '=zero? z))

;; Scheme-number package
(put '=zero? '(scheme-number)
     (lambda (z) (= z 0)))

;; Rational package
(put '=zero? '(rational)
     (lambda (z) (= (numer z) 0)))

;; Complex package
(put '=zero? '(complex)
     (lambda (z) (= (magnitude z) 0)))

