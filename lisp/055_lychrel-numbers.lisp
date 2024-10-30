(defun is-lychrel (n)
  (let ((cur-num n))
    (dotimes (i 50)
      (incf cur-num (reverse-num cur-num))
      (when (palindromep cur-num)
	(return-from is-lychrel nil))))
  t)

(loop for i from 1 to 9999
      count (is-lychrel i))