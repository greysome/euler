;; Diagonal numbers are partial sums of the infinite series
;; 3+2+2+3+3+4+4+5+5+...
(let ((n 3)
      (num-diagonals 1)
      (num-primes 0)
      (gap 2)
      (steps-till-increase-gap 3))
  (loop
   (when (primep n)
     (incf num-primes))
   (incf num-diagonals)
   (if (zerop steps-till-increase-gap)
       (progn
	 (when (and (oddp (isqrt n))
		    (< (/ num-primes num-diagonals) 0.1))
	   (return (isqrt n)))
	 (incf gap 2)
	 (setf steps-till-increase-gap 3))
       (decf steps-till-increase-gap))
   (print2 n num-diagonals (/ num-primes num-diagonals))
   (incf n gap)))