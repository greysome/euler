(defun letter-value (c)
  (1+ (- (char-code c) (char-code #\A))))

(defun word-value (word)
  (loop with value = 0
	for c across word
	do (incf value (letter-value c))
	finally (return value)))

(defvar *names* (make-array 5163 :initial-element "" :element-type 'string))

(let ((in (open "p022_names_transformed.txt")))
  (do ((line (read-line in nil) (read-line in nil))
       (line-num 0 (1+ line-num)))
      ((not line))
    (setf (elt *names* line-num) line)))

(sort *names* #'string-lessp)

(let ((total 0))
  (doenum (name pos *names*)
    (incf total (* (1+ pos) (word-value name))))
  total)

(loop with total = 0
      for pos from 0 to 5162
      for name = (elt *names* pos)
      do (incf total (* (1+ pos) (word-value name)))
      finally (return total))