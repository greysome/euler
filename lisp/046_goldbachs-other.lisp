(defvar *corrects*)
(setq *corrects* (make-array 10000 :initial-element nil))

(doranges ((i 1 10000)
	   (j 1 (1+ (isqrt
		     (ceiling (/ (- 10000 i) 2))))))
  (when (primep i)
    (let ((n (+ i (* 2 (* j j)))))
      (when (< n 10000)
	(setf (elt *corrects* n) t)))))

(loop for i from 2 to (dec 10000)
      do (when (and (oddp i)
		    (not (primep i))
		    (not (elt *corrects* i)))
	   (format t "~d~%" i)
	   (return)))