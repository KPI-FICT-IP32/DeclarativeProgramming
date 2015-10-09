(defun str_from_list (lst)
  (cond 
    ((null lst) lst)
    (t (eval (cons 'concatenate (cons ''string lst))))))

(defun str_contain(str character) 
  (cond
    ((or (< (length str) 1) (null character)) nil)
    ((equal (subseq str 0 1) character) T)
    (T (str_contain (subseq str 1) character))))

(defun symb_from_list (word elements)
  (cond 
    ((or (null word) (null elements)) nil)
    (T (if 
         (str_contain word (car elements)) T
         (symb_from_list word (cdr elements))))))

(defun extract_syll (word slog) 
  (cond
    ((< (length word) 1) (values slog word))
    ((null slog) (extract_syll (subseq word 1) (subseq word 0 1)))
    (T (let (
             (vowels '("а" "у" "е" "и" "і" "ї" "о" "я" "ю"))
             (consonants '("й" "ц" "к" "н" "г" "ш" "щ" "з" "х" "ф" "в" "п" "р" "л" "д" "ж" "ч" "с" "м" "т" "б"))
             (resonant '("м" "н" "р" "л" "й"))
             (special '("ь")))
             (if (symb_from_list slog vowels)
               (if (symb_from_list word vowels)
                 (cond 
                   ((and 
                      (symb_from_list (subseq word 0 1) resonant)
                      (symb_from_list (subseq word 1 2) consonants) 					
                      (not (equal (subseq word 0 1) (subseq word 1 2))))
                    (values (str_from_list (concatLists slog (subseq word 0 1))) (subseq word 1)))
                   ((and 
                      (symb_from_list (subseq word 0 1) resonant)
                      (symb_from_list (subseq word 1 2) special)
                      (symb_from_list (subseq word 2 3) consonants))
                    (values (str_from_list (concatLists slog (subseq word 0 2))) (subseq word 2)))
                   (T (values slog word)))
                 (values (str_from_list (concatLists slog word)) nil))
               (extract_syll 
                 (subseq word 1) 
                 (str_from_list (concatLists slog (subseq word 0 1)))))))))

(defun syll_list (word)
  (cond
    ((< (length word) 1) nil)		
    (T (multiple-value-bind 
         (new_syll new_word) 
         (extract_syll word ()) 
         (cons new_syll (syll_list new_word))))))

(defun syll_from_sent  (phrase)
  (mapcar 'syll_list (car (parse_string phrase))))

