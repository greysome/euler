(defvar *primes*)
(setq *primes* (make-array 78498 :fill-pointer 0))
(loop for i from 2 to 1000000
      do (when (primep i)
	   (vector-push i *primes*)))

(let ((n (length *primes*))
      (maxlen 0)
      (maxsum 0))
  (doranges ((i 2 n) (j 0 (- n i)))
    (when (= j 0)
      (format t "~d ~d ~d~%" i maxlen maxsum))
    (let ((sum
	    (loop for k from j to (dec (+ j i))
		  sum (elt *primes* k))))
      (unless (or
	       (>= sum 1000000)
	       (not (primep sum)))
	(when (> i maxlen)
	  (setq maxlen i
		maxsum sum)))))
  (format t "~d ~d~%" maxlen maxsum))