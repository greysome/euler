(defmacro nappend (lst1 lst2)
  `(setq ,lst1 (append ,lst1 ,lst2)))

(defmacro push-to-all (item lsts)
  `(mapcar (lambda (x) (push ,item x))
	   ,lsts))

(defmacro branch (n)
  `(when (and (>= amount ,n) (>= max-denom ,n))
     (nappend result
	      (push-to-all ,n
			   (get-ways (- amount ,n) ,n)))))

(defun get-ways (amount max-denom)
  (when (zerop amount)
    (return-from get-ways '(())))
  (let ((result '()))
    (branch 1)
    (branch 2)
    (branch 5)
    (branch 10)
    (branch 20)
    (branch 50)
    (branch 100)
    (branch 200)
    (return-from get-ways result)))

(length (get-ways 200 200))