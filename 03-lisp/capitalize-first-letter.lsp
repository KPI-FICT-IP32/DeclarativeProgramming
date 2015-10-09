(defun capitalize-first-letter (text) 
  (map 
    'list 
    (lambda (x) (cons (string-capitalize (car x)) (cdr x))) 
    text))
