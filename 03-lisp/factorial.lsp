(defun range (a &optional b) 
  (flet 
    ((rng (mm mx)
          (cond 
            ((= mm mx) nil) 
            (t (cons mm (range (+ mm 1) mx))))))
  (cond
    ((null b) (rng 0 a))
    (t (rng a b)))))

(defun factorial-recursive (n)
  (cond 
    ((< n 2) 1)
    (t (* n (factorial-recursive (- n 1))))))

(defun factorial-functional (n)
  (reduce #'* (range 1 (+ n 1))))
