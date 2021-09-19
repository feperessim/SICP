(define (equ? x y)
  (apply-generic 'equ? x y))

;; Scheme-number package
(put 'equ? '(scheme-number scheme-number) =)

;; Rational package
(put 'equ? '(rational rational)
     (lambda (x y) (and (= (numer x) (numer y))
			(= (denom x) (denom y)))))

;; Complex package
(put 'equ? '(complex complex)
     (lambda (x y) (and (= (real-part x) (real-part y))
			(= (imag-part x) (imag-part y)))))

