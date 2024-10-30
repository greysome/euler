;; 1,2 - 4,5. exactly 4 digits
;; 1,2,3 - 3,3,3 or 2,3,4. at most 4 digits
;; 1,2,3,4 - 2,2,2,3. at most 2 digits
;; 1,2,3,4,5 - at most 1 digits
;; 1,2,3,4,5,6 - at most 1 digit
;; 1,2,3,4,5,6,7 - at most 1 digit
;; 1,2,3,4,5,6,7,8 - at most 1 digit

(defun pandigitalp (lst)
  (and
   (not (member 0 lst))
   (= (length lst) 9)
   (= (length (unique lst)) 9)))

;; return the set of digits contained in the products
;; num, num*2, ..., num*n
(defun concat-product (num n)
  (apply #'append
	 (mapcar (lambda (x) (digits (* x num)))
		 (loop for i from 1 to n collect i))))

(do* ((largest 0)
      (n 2 (1+ n))
      (i (floor (/ 9 n)) (floor (/ 9 n))))
     ((>= n 9) largest)
  (dorange (num 1 (expt 10 i))
    (let ((prod (concat-product num n)))
      (when (and (pandigitalp prod)
		 (> (from-digits prod) largest))
	(format t "~d ~d~%" num n)
	(setf largest (from-digits prod))))))