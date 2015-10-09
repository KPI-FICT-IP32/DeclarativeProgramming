(defun range (a &optional b) 
  (labels 
    ((rng (mm mx)
          (cond 
            ((= mm mx) nil) 
            (t (cons mm (range (+ mm 1) mx))))))
  (cond
    ((null b) (rng 0 a))
    (t (rng a b)))))


