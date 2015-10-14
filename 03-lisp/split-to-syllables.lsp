(defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
        as j = (position #\Space string :start i)
        collect (subseq string i j)
        while j))

(defun str-from-list (lst)
  (cond 
    ((null lst) lst)
    (t (eval (cons 'concatenate (cons ''string lst))))))

(defun str-contain(str letter) 
  (let ((str (string-downcase str)))
    (cond
      ((or (< (length str) 1) (null letter)) nil)
      ((equal (subseq str 0 1) letter) T)
      (T (str-contain (subseq str 1) letter)))))

(defun symb-from-list (word elements)
  (cond 
    ((or (null word) (null elements)) nil)
    (T (if 
         (str-contain word (car elements)) T
         (symb-from-list word (cdr elements))))))

(defun extract-syll (word syllable) 
  (cond
    ((< (length word) 1) (values syllable word))
    ((null syllable) (extract-syll (subseq word 1) (subseq word 0 1)))
    (T (let (
             (vowels '("а" "у" "е" "и" "і" "ї" "о" "я" "ю" "э" "ё" "ы"))
             (consonants '("й" "ц" "к" "н" "г" "ш" "щ" "з" "х" "ф" "в" "п" "р" "л" "д" "ж" "ч" "с" "м" "т" "б" "ґ"))
             (resonant '("м" "н" "р" "л" "й"))
             (special '("ь" "ъ")))
         (if (symb-from-list syllable vowels)
           (if (symb-from-list word vowels)
             (cond 
               ((and 
                  (symb-from-list (subseq word 0 1) resonant)
                  (symb-from-list (subseq word 1 2) consonants) 					
                  (not (equal (subseq word 0 1) (subseq word 1 2))))
                (values (str-from-list (list syllable (subseq word 0 1))) (subseq word 1)))
               ((and 
                  (symb-from-list (subseq word 0 1) resonant)
                  (symb-from-list (subseq word 1 2) special)
                  (symb-from-list (subseq word 2 3) consonants))
                (values (str-from-list (list syllable (subseq word 0 2))) (subseq word 2)))
               (T (values syllable word)))
             (values (str-from-list (list syllable word)) nil))
           (extract-syll 
             (subseq word 1) 
             (str-from-list (list syllable (subseq word 0 1)))))))))

(defun syll-list (word)
  (cond
    ((< (length word) 1) nil)		
    (T (multiple-value-bind 
         (new-syll new-word) 
         (extract-syll word ()) 
         (cons new-syll (syll-list new-word))))))

(defun syll-from-sent  (phrase)
  (mapcar 'syll-list (split-by-one-space phrase)))

