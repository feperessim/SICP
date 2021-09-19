;; This interchange makes the program run slow because
;; in Louis implementation flatmap maps each element
;; of the list produced by enumerate-interval instead
;; rest-of-queens, and so for each element there is a
;; recursive call for queens-cols and for each recursive
;; call there is another enumerate-interval and more
;; n recursive calls for queens-cols. Given that there
;; the board has n rows and n cols, the procedure
;; queens-cols will be called n^n times, meanwhile
;; in the book's implementation it is only called
;; n times. Therefore the puzzle takes T^n time
;; to solve the puzzle.
