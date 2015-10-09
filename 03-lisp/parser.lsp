(defun to-str (&rest args)
  (reduce 
    (lambda (x y) 
      (concatenate 'string x y))
      (map 'list #'string args)))

(defun parse (src) 
    (let ((head (car src)) (tail (cdr src))) 
         (cond
           ((eq head 'quote) (list  src))
           (t (cond
                ((null src) nil)
                ((numberp head) (cons head (parse tail)))
                ((listp head)  (cons (parse head) (parse tail)))
                ((fboundp head) (funcall head (parse tail)))
                (t (parse tail)))))))

