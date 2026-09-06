(define (make-account balance password)
  (define pass-counter 0)
  (define (call-the-cops)
    (if (>= pass-counter 7)
        (error "Calling the cops")))
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insuficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (if (eq? password p)
        (begin
          (set! pass-counter 0)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "Unknown request -- MAKE-ACCOUNT"
                             m))))
        (begin
          (set! pass-counter (+ pass-counter 1))
          (call-the-cops)
          (error "Incorrect password"))))
  dispatch)


(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40) ;; => 60
((acc 'some-other-password 'withdraw) 50) ;; => Incorrect password


  
