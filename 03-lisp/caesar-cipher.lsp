(defun encipher-char (ch key)
  (let*
    ((c  (char-code  ch))
     (la (char-code #\a)) (lz (char-code #\z))
     (ua (char-code #\A)) (uz (char-code #\Z))
     (base (cond 
             ((and (>= c la) (<= c lz)) la) 
             ((and (>= c ua) (<= c uz)) ua) 
             (t nil))))
    (if base (code-char (+ (mod (+ (- c base) key) 26) base)) ch)))

(defun caesar-cipher (str key)
  (map 'string #'(lambda (c) (encipher-char c key)) str))

(defun caesar-decipher (str key) (caesar-cipher str (- key)))
