;; Equivalente algebraic expressions may lead to different answers due machine arithmetic precision on floating point numbers,
;; Real numbers cannot be represented in a computer since they have infinity decimal places. Float point numbers are implemented
;; in real computers to represent real numbers with limited precision.
;; When an arithmetic operation on floating point numbers reach its bounds in a computer, it is implemented to return some arbitrary
;; value as a zero or an negative value, or any other kind of value. As computer doesn't operate on fractional arithmetic, some
;; arithmetic operations lead to very small numbers which may supass the machine precision, which leads the programmer to
;; use equivalent algebraic formula that avoids of producing too small or too large numbers, to avoid erros at numerical computation.

;; I believe that is possible to devise an interval-arithmetic package where diferente but equivalent algebraic formulas lead to the same
;; answer. Although I am unable to do it right now, the warning at the exercise discouraged me.