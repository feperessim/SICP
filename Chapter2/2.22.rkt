;; The answer is produced in reverse order because it is growing to the right.
;; The first item is squared and saved into the list as (cons first_item squared nil),
;; the next item would be (cons second_item (cons first_item squared nil)), since answer
;; is passed in every iteraction it goes until reach the end of the original list yielding
;; (cons nth_item (cons .... (cons third_item (cons first_item squared nil)))) hence it produces
;; the squared list in reverse order.

;; The second doesn't work either because the second argument of should be a list and nil is appended at the tail
;; of the list, not at the head.

