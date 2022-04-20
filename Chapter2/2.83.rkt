(define (raise arg)
  (apply-generic 'raise arg))

(put 'raise 'integer
     (lambda (arg) (make-rational arg 1)))

(put 'raise 'rational
     (lambda (arg) (make-real (/ (number arg) (denom arg)))))

(put 'raise 'real
     (lambda (arg) (make-from-real-imag arg 0)))
