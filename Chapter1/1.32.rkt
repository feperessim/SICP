;; Recursive accumulate process

(define (accumulate combiner null-value term a next b)
  (if (> a  b)
      null-value
      (combiner a
                (accumulate
                 combiner
                 null-value
                 term (next a) next b))))

;; Iterative accumulate process

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a  b)
      result
      (iter (next a)
            (combiner (term a) result))))
  (iter a null-value))

