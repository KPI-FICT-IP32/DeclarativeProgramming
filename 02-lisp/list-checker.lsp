(defun list-checker (lst1 lst2 depth)
  (cond 
    ((or (null lst1) (not (listp lst1))) nil)
    ((= depth 0) (equal lst1 lst2))
    (t (cond
      ((null (list-checker (car lst1) lst2 (- depth 1))) (list-checker (cdr lst1) lst2 depth))
      (t t)))))
