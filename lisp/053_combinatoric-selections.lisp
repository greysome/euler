(let ((num-values 0))
  (dorange (n 1 101)
    (dorange (r 0 (1+ n))
      (when (> (nCr n r) 1000000)
	(incf num-values (- (1+ n) (* 2 r)))
	(return))))
  num-values)
