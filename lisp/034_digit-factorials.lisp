(* 8 (factorial 9)) ;; 2903040
;; thus all 8-digit numbers have sum of factorial of digits
;; <= 7 digits

(defun f (n)
  (apply #'+ (mapcar #'factorial (digits n))))

(loop for i from 1 to 9999999
      when (= (f i) i)
	do (format t "~d ~%" i)
      when (zerop (mod i 200000))
	do (format t ">~d ~%" i))

(+ 145 40585)
