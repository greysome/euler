(defun possible-steps (n)
  (loop for p in (permutations (digits n))
	for step = (- (from-digits p) n)
	when (and
	      (> step 0)
	      (< (+ n step step) 10000))
	  collect step))

(defun same-digits (m n)
  (same-elements (digits m) (digits n)))

(dorange (i 1000 9999)
  (dolist (step (possible-steps i))
    (let ((j (+ i step))
	  (k (+ i step step)))
      (when (and
	     (same-digits j k)
	     (primep i)
	     (primep j)
	     (primep k))
	(format t "~d ~d ~d~%" i j k)))))
