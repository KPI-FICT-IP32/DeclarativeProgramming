(defun count-nested-lists(lst)
  (cond
    ((not lst) 0)
    ((listp (car lst))
     (+ 
       (count-nested-lists (car lst))
       (count-nested-lists (cdr lst))
       1))
    (t (count-nested-lists (cdr lst)))))
