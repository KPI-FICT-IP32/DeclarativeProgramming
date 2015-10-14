(defun to-str (&rest args)
  (reduce 
    (lambda (x y) 
      (concatenate 'string x y))
    (map 'list #'string args)))

(defun interpret (src) 
  (cond 
    ((atom src) src)
    (t (let (
             (head (car src)) 
             (tail (cdr src)))
         (cond
           ((eq head 'quote) (car tail))
           ((numberp head) (cons head (interpret tail)))
           ((listp head) (cons (interpret head) (interpret tail)))
           ((fboundp head) (apply head (interpret tail)))
           (t (interpret tail)))))))

