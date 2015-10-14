(defun make-item (type l-mm b-mm h-mm c-mm d-mm e-mm power mass magnetwire)
  (list
    :type type
    :l-mm l-mm
    :b-mm b-mm
    :h-mm h-mm
    :c-mm c-mm
    :d-mm d-mm
    :e-mm e-mm
    :power power
    :mass mass
    :magnetwire magnetwire
    ))

(defvar *db* nil)

(defun db-insert (item)
  (push item *db*))

(defun db-select-one (type)
  (remove-if-not
    #'(lambda (item) (equal (getf item :type) type))
    *db*))

(defun db-select-all ()
  (dolist (item *db*)
    (format t "~{~a:~10t~a~%~}~%" item)))

(defun db-delete (type)
  (setf
    *db*
    (remove-if
      (lambda (item) (equal (getf item :type) type)) *db*)))

(defun db-update (item)
  (db-delete (getf item :type))
  (db-insert item))

(defun db-save (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun db-load (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))


