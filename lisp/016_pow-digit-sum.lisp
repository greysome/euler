(let* ((n (expt 2 1000)))
  (loop for i from 0 to (1+ (num-digits n))
	sum (digit n i)))