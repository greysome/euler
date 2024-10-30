(let ((max 0) (argmax 0))
  (doranges ((i 1 100) (j 1 100))
    (let ((sum (sum-digits (expt i j))))
      (when (> sum max)
	(setq max sum
	      argmax (list i j)))))
  (format "~a ~a~%" max argmax))