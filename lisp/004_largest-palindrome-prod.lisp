(let ((max-palindrome 0) (max-i 0) (max-j 0))
  (dorange (i 999 99 -1)
    (when (< (* i i) max-palindrome)
      (format t "~d = ~d * ~d" max-palindrome max-i max-j)
      (return))
    (dorange (j i 99 -1)
      (let* ((prod (* i j)))
	(when (and (palindromep prod)
		  (> prod max-palindrome))
	  (setf max-palindrome prod
		max-i i
		max-j j))))))