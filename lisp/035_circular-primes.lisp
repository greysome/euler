(defun from-digits (digits)
  (loop with exponent = 0
	for i in digits
	sum (* i (expt 10 exponent))
	do (incf exponent)))

(defun rotate-list (lst)
  (let ((len (length lst)))
    (cons (nth (dec len) lst)
	  (subseq lst 0 (dec len)))))

(defun circular-prime-p (n)
  (let ((num-digits (num-digits n))
	(digits (digits n))
	(rotated-versions '()))
    (dotimes (i num-digits)
      (unless (primep (from-digits digits))
	(return-from circular-prime-p (values nil nil)))
      (pushnew (from-digits digits) rotated-versions)
      (setq digits (rotate-list digits)))
    (values t rotated-versions)))

(defvar *circulars* '())

(dorange (i 2 1000000)
  (multiple-value-bind (circular-prime-p rotated-versions)
      (circular-prime-p i)
    (when (and circular-prime-p
	       (not (member i *circulars*)))
      (format t "~d ~a~%" i *circulars*)
      (setq *circulars* (nconc *circulars* rotated-versions)))))

(length *circulars*)