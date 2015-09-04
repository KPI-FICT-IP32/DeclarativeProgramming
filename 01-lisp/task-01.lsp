;;; TASK 01
;;;
;;; Write an unnamed-function which creates a list from
;;; heads of arguments

;; Basic idea in this implementation:
;; 1. Create a list of lists which should be processed
;; 2. Use mapcar function to apply lambda to list from step 1
;; 3. mapcar returns a resulting list


;; Variant 13 (gradebook number is  #3213)
;; Sequences:
;;  seq1: '(REM FFG HHJ (H )J G D)
;;  seq2: '(2 34 56 78 (7 8))
;;  seq3: '(UN Y LOOP)

(
  (lambda (&rest lists)
    (mapcar
      (lambda (x) (car x))
      lists
    )
  )
  '(REM FFG HHJ (H )J G D)
  '(2 34 56 78 (7 8))
  '(UN Y LOOP)
)
