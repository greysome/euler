(defvar *f*)
(setf *f* (open "p042_words.txt"))
(defvar *str*)
(setf *str* (read-line *f*))
(defvar *words*)
(setf *words* (split-sequence #\, *str*))
(setf *words*
      (mapcar (lambda (x) (subseq x 1 (dec (length x))))
	      *words*))

(defun letter-value (letter)
  (1+ (- (char-code letter) (char-code #\A))))

(defun word-value (word)
  (loop for letter across word
	sum (letter-value letter)))

(loop for word in *words*
      count (trianglep (word-value word)))