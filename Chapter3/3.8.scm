(define f
  ((lambda ()
     (let ((tmp '()))
     (lambda (val)
       (if (null? tmp)
           (begin
             (set! tmp val) 0)
           tmp))))))
