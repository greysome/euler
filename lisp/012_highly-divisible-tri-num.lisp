(defun num-divisors (n)
  (loop with prod = 1
	for (prime . multiplicity) in (factorize n)
	do
	   (setf prod (* prod (1+ multiplicity)))
	finally (return prod)))

(do ((k 2 (1+ k)))
    (nil)
  (let ((n (/ (* k (- k 1))
	      2)))
    (when (> (num-divisors n) 500)
      (return n))))