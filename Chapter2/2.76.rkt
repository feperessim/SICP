;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic operations generic operations with explicit dispatch: ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;
;; New types ;;
;;;;;;;;;;;;;;;
;; To add a new type we would need to add a clause to each
;; of the generic interface procedures to check for the new
;; type and apply the apropriated selector for that representation.

;;;;;;;;;;;;;;;;;;;;
;; New operations ;;
;;;;;;;;;;;;;;;;;;;;
;; Procedures would have to take in account each existing type
;; in order to know which operations to apply according to the types
;; We also need to guarantee that no two procedures have the same
;; name

;;;;;;;;;;;;;;;;;;;
;; Data-directed ;;
;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;
;; New types ;;
;;;;;;;;;;;;;;;

;; To add new types we only need to add new entries to the table
;; which means that we need to add the corresponding new operations
;; corresponding to the new type to the table.

;;;;;;;;;;;;;;;;;;;;
;; New operations ;;
;;;;;;;;;;;;;;;;;;;;
;; One procedure for each data type must be added to the table
;; to perform the operation


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Message-passing-style: ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;
;; New types ;;
;;;;;;;;;;;;;;;
;; Adding a new type requires to implement a new dispatch procedures
;; with all operations.
;;
;;;;;;;;;;;;;;;;;;;;
;; New operations ;;
;;;;;;;;;;;;;;;;;;;;
;; When a new operation is needed, each data type must alter
;; its dispatch procedure to take into account the new operations.

;; Data directed programming seems the most suitable to use in a system
;; that requires a lot to install new operations.
