;;; TASK 02
;;;
;;; Write named function that accepts lists and list of numbers
;;; and then creates a list from elements at corresponding positions
;;; Example:
;;; '(A B C) '(D E F) '(G H I) '(1 2 0) => (B F G)

;; For convenience positions will go first

(defun combine_lists (positions &rest lists)
  (if
    (=
      (length positions)
      (length lists)
    )
    (mapcar
      (lambda (x)
        (nth
          (nth
            (position x lists)
            positions
          )
          x
        )
      )
      lists
    )
    NIL
  )
)

;; Usage:
;; (combine_lists
;;  '(4 4 2)
;;  '(REM FFG HHJ (H )J G D)
;;  '(2 34 56 78 (7 8))
;;  '(UN Y LOOP)
;; )
