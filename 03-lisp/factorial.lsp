(load 'funclib.lsp)

(defun factorial-recursive (n)
  (cond 
    ((< n 2) 1)
    (t (* n (factorial-recursive (- n 1))))))

(defun factorial-functional (n)
  (reduce #'* (range 1 (+ n 1))))
