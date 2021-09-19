(define (make-vec x y)
  (cons x y))

(define (xcor-vect vect)
  (car vect))

(define (ycor-vect vect)
  (cdr vect))

(define (add-vect u w)
  (make-vec
   (+ (xcor-vect u)
      (xcor-vect w))
   (+ (ycor-vect u)
      (ycor-vect w))))

(define (sub-vect u w)
  (make-vec
   (- (xcor-vect u)
      (xcor-vect w))
   (- (ycor-vect u)
      (ycor-vect w))))

(define (scale-vect scalar vector)
  (make-vec
   (* scalar (xcor-vect vector))
   (* scalar (ycor-vect vector))))
      

      
