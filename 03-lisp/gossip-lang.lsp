(load 'split-to-syllables.lsp)

(defun encode-word (word key)
  (let
    ((syllables-word (syll-list word))
     (syllables-key (syll-list key)))
    (list
      (string-concat
        (car syllables-key)
        (reduce (lambda (x y) (string-concat x y)) (cdr syllables-word)))
      (string-concat
        (car syllables-word)
        (reduce (lambda (x y) (string-concat x y)) (cdr syllables-key))))))

(defun encode-text (text key)
  (reduce 
    (lambda (x y) (string-concat x " " y))    
    (map 'list
      (lambda (x) (string-concat (car x) " " (cadr x)))
      (map 'list 
        (lambda (word) (encode-word word key)) 
        (split-by-one-space text)))))
