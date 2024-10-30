(let ((result '()))
  (doranges ((a 2 101) (b 2 101))
	    (pushnew (expt a b) result))
  (length result))
