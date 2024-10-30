(defmacro pythagorean-triple-p (a b c)
  `(= (+ (* ,a ,a) (* ,b ,b)) (* ,c ,c)))

(block loops
  (doranges ((i 1 1000)
	     (j (1+ i) (- 1000 i)))
    (let ((k (- 1000 i j)))
      (when (<= k j)
	(return))
      (when (pythagorean-triple-p i j k)
	(return-from loops (* i j k))))))