;; Since calling square was halving the problem turning it in a log(n) problem,
;; now the expmod is being called twice to do the multiplications thus canceling
;; instead of only one. Thus a single value is computed twice now, even the exp
;; being halved, as long as the expmod is called twice the halve is turning out
;; to be one, because 1/2 + 1/2 = 1, thus transforming this procedure into  theta of n.