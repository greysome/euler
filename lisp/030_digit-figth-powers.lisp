(defun sum-fifth-power-digits (n)
  (apply #'+ (mapcar (lambda (x) (expt x 5))
		     (digits n))))

(loop with sum = 0
      for i from 10 to 999999
      when (= i (sum-fifth-power-digits i))
	do (incf sum i)
      finally (return sum))