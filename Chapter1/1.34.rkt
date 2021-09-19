(define (f g)
  (g 2))

(f f)

in the body of f it will attemp to apply f to 2, which will finnaly try to apply 2 to 2 as shown below, resulting in an error captured by the interpreter.

(f f)
(f 2)
(2 2)

application: not a procedure;
 expected a procedure that can be applied to arguments
  given: 2
  arguments...:

  The error above was given by Racket version 6.5, using the #lang SICP package