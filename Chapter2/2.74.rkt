;; ###########
;; #    1    #
;; ###########
;; 
;; The  Insatiable Enterprises, Inc., needs to
;; install the division name and the generic
;; procedure get-record into the lookup table


(define (get-record personel-file employee-name)
  ((get 'get-record (division-name personel-file))
   (file personel-file) employee-name))


;; ###########
;; #    2    #
;; ###########
;; The record must contain the division name
;; and be indexed by employee-name

(define (get-salary personel-file employee-name)
  ((get 'get-salary (division-name personel-file))
   (file personel-file) employee-name))


;; ###########
;; #    3    #
;; ###########


(define (find-employee-record employee-name division-files)
  (if (null? division-files)
      #f
      (let ((record (get-record (car division-files)  employee-name)))
	(if (null? record)
	    (find-employee-record employee-name (cdr division-files))
	    record))))


;; ###########
;; #    4    #
;; ###########
;;
;; They need to implment their record data structure representation
;; and install it in the lookup table. Which means, they need to install
;; their selectors into the table

