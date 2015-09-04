;;; TASK 03
;;;
;;; Assume we have two lists. 
;;; If the product of first elements of these lists is positive
;;; the result list consists of last elements of lists. Otherwise
;;; it consists of last element of the first list and tail of the second list

(defun conditional_combine(list1 list2)
  (if
    (>
      (*
        (car list1)
        (car list2)
      )
      0
    )
    (cons
      (car (last list1))
      (car (last list2))
    )
    (cons
      (car (last list1))
      (cdr list2)
    )
  )
)
