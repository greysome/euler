(defmacro eval-quadratic (a b n)
  `(+ (* ,n ,n) (* ,a ,n) ,b))

(defun num-consecutives (a b)
  (do* ((n 0 (1+ n))
	(y (eval-quadratic a b n) (eval-quadratic a b n)))
       ((or (<= y 1)
	    (not (primep y)))
	n)))

(let ((max-a 0) (max-b 0) (max-consecutives 0))
  (doranges ((a -999 1000) (b -1000 1001))
    (when (zerop b)
      (format t "~d~%" a))
    (let ((c (num-consecutives a b)))
      (when (> c max-consecutives)
	(setq max-consecutives c
	      max-a a
	      max-b b))))
  (values max-a max-b max-consecutives))
