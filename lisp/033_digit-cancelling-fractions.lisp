(defmacro safediv (a b)
  `(if (zerop ,b) 0 (/ ,a ,b)))

(defmacro branch (x1 y1 x2 y2)
  `(and (= ,x1 ,y1)
	(= (/ num denom) (safediv ,x2 ,y2))))

(doranges ((a 1 10) (b 0 10) (c 1 10) (d 0 10))
  (let ((num (+ (* 10 a) b))
	(denom (+ (* 10 c) d)))
    (when (and (< num denom)
		(or (branch a c b d)
		    (branch a d b c)
		    (branch b c a d)
		    (branch b d a c))
		(not (and (zerop b) (zerop d))))
      (format t "~d/~d~%" num denom))))

(* 16/64 19/95 26/65 49/98)