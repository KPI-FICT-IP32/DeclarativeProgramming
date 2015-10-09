;;; (splt 'kontti) -> (#\K #\O#\N #\T #\T #\I)

(defun splt (word)
  (spltword nil
             (coerce (string word) 'list)))

(defun spltword (begining ending)
  (cond
    ((null ending) (list begining ending))
    ((is-consonant (first ending))
     (spltword
       (vending begining (first ending))
       (rest ending)))
    ((dolgaianach ending)
     (list (append begining
                   (list (first ending)
                         (second ending)))
           (cddr ending)))
    (t (list (vending begining (first ending))
             (rest ending)))))

(defun vending (lst element)
  (append lst (list element)))

(defun is-vowel (bukva)
  (member bukva *glasnie*))
(setq *glasnie* '(#\A #\E #\I #\O #\U #\Y #\a #\o))
(defun is-consonant (bukva)
  (not (is-vowel bukva)))
(defun dolgaianach (word)
  (and (is-vowel (first word))
       (eql (first word)
            (second word))))

;;; (per-word 'sana 'kontti)
;;; -> (kona santti)

(defun perword (word kluch)
  (let ((chastiword (splt word))
        (chastiklucha (splt kluch)))
    (dolgglas (first chastiword)
              (second chastiword)
              (first chastiklucha)
              (second chastiklucha))))

(defun dolgglas (begining1 ending1 begining2 ending2)
  (cond ((dolgaiakon begining1)
         (cond ((dolgaiakon begining2)
                (pomchasti begining2 ending1 begining1 ending2))
               (t (pomchasti
                    (shorten begining1) ending1
                    (longer begining2) ending2))))
        ((dolgaiakon begining2)
         (pomchasti
           (longer begining1) ending1
           (shorten begining2) ending2))
        (t (pomchasti begining2 ending1 begining1 ending2))))

(defun dolgaiakon (word)
  (dolgaianach (reverse word)))

(defun shorten (syllable)
  (if (not (rest syllable))
    nil
    (cons (first syllable)
          (shorten (rest syllable)))))

(defun longer (syllable)
  (if (null (rest syllable))
    (cons (first syllable) slog)
    (cons (first syllable)
          (longer (rest syllable)))))

(defun pomchasti (begining1 ending1 begining2 ending2)
  (list (soz begining1 ending1)
        (soz begining2 ending2)))

(defun soz (begining ending)
  (cond ((pered begining)
         (soedeni begining (forward ending)))
        (t (soedeni begining (backwards ending)))))

(defun pered (word)
  (intersection word *perglas*))
(setq *perglas* '(#\Y #\a #\o))

(defun forward (word)
  (sublis '((#\U . #\Y) (#\A . #\a) (#\O . #\o)) word))

(defun backwards (word)
  (sublis '((#\Y . #\U) (#\a . #\A) (#\o . #\O)) word))

(defun soedeni (begining ending)
  (intern (coerce (append begining ending)
                  'string)))

(defun perpr
  (pr kluch)
  (if (null pr) nil
    (append (perword (first pr) kluch)
            (perpr (rest pr) kluch))))

(defun nacig (pr)
  (cond
    ((null pr) nil)
    ((null (rest pr))
     (perword
       (first pr) 'kontti))
    (t (append
         (perword
           (first pr)
           (second pr))
         (nacig
           (cddr pr))))))

(perword 'sana 'kontti)
