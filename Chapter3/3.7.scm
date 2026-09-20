(define (make-account balance password)
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
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        (error "Incorrect password")))
  dispatch)

(define (make-joint acc first-pw second-pw)
  (define (dispatch p m)
    (if (eq? second-pw p)
        (acc first-pw m)
        (error "Incorrect password")))
  dispatch)

(define peter-acc (make-account 100 'open-sesame))
((peter-acc 'open-sesame 'withdraw) 40) ;; => 60
(define paul-acc (make-joint peter-acc 'open-sesam 'rosebud)) ;; => #Incorrect password
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud)) ;; => #<unspecified>
((paul-acc 'rosebud 'withdraw) 50) ;; => 10
((peter-acc 'open-sesame 'withdraw) 10) ;; => 0

