(defun merge-lists (lst1 lst2)
  (cond 
    ((null (or lst1 lst2)) nil)
    ((null lst1) lst2)
    ((null lst2) lst1)
    ((> (car lst1) (car lst2))
     (cons (car lst2) (merge-lists lst1 (cdr lst2))))
    (t (cons (car lst1) (merge-lists lst2 (cdr lst1))))))
