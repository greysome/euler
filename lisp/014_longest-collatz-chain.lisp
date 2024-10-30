(defun collatz-next (n)
  (if (evenp n)
      (/ n 2)
      (1+ (* 3 n))))

(defun collatz-len (n)
  (do ((len 1 (1+ len))
       (cur n (collatz-next cur)))
      ((= cur 1) len)
    ))

(let ((max-len 0) (max-num 0))
  (dorange (i 1 1000000)
    (let ((cur-len (collatz-len i)))
      (when (> cur-len max-len)
	(setq max-len cur-len
	      max-num i))))
  max-num)