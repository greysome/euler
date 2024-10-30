(defun f (n)
  (+ (* 4 (+ 3
	     (* 3 n (- n 1))
	     (* n (+ n 1))
	     -2))
     (* 12 n)))

(1+ (loop for i from 1 to 500
	  sum (f i)))