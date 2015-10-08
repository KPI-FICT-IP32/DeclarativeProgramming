(defun insert (item lst &optional (key #'<))
  (cond
    ((null lst) (list item))
    (t 
      (cond 
        ((funcall key item (car lst)) (cons item lst))
        (t (cons (car lst) (insert item (cdr lst) key)))
      )
    )
  )
)

(defun insertion-sort (lst &optional (key #'<))
  (cond
    ((null lst) lst)
    (t (insert (car lst) (insertion-sort (cdr lst) key) key))
  )
)
