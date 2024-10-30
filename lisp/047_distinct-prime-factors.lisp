(defun my-condition-aux (n)
  (= (length (factorize n)) 4))

(defun my-condition (n)
  (and
   (my-condition-aux n)
   (my-condition-aux (+ n 1))
   (my-condition-aux (+ n 2))
   (my-condition-aux (+ n 3))))

(loop for i from 1 to 1000000
      do (when (my-condition i)
	   (format t "~d~%" i)
	   (return)))