(define (p) (p))

(define (test x y) 
  (if (= x 0) 
      0 
      y))
;; Then he evaluates the expression

(test 0 (p))
;; What behavior will Ben observe with an interpreter that uses applicative-order evaluation?
;; What behavior will he observe with an interpreter that uses normal-order evaluation? Explain your answer.
;; Assume that the evaluation rule for the special form if is the same whether the interpreter is using normal
;; or applicative order: The predicate expression is evaluated first, and the result determines whether to evaluate
;; the consequent or the alternative expression.)

;; In the applicative-order evalutation the interpreter first evaluates the and operand and the applies the resulting
;; procedure to the resulting arguments. Thus when the interpreter evaluate the compound expressions into the body of the procedure test
;; and replaced the arguments by values given x would evaluate to 0 and y to the procedure (p) where the interpreter would then apply
;; the body of p which would call itself indefinitly leading to a infinity loop.

;; In the normal order it would not evaluate the operands untill their values were needed. Thus in this case as (if (= x 0) 0 y) would evaluates
;; to true when x were tested against 0 and y would be never evaluated resulting in 0 as the answer given by the procedure test
