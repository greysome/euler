(defun pentagonal (n)
  (/ (* n (dec (* 3 n))) 2))

(defun pentagonalp (n)
  (and (squarep (1+ (* 24 n)))
       (divisiblep (1+ (isqrt (1+ (* 24 n)))) 6)))

(defun my-cond (j k)
  (let ((Pj (pentagonal j))
	(Pk (pentagonal k)))
    (and
     (pentagonalp (+ Pj Pk))
     (pentagonalp (- Pk Pj)))))

(doranges ((i 2 100000) (j 1 (/ i 2)))
  (let ((k (- i j)))
    (when (my-cond j k)
      (format t "~a = ~a + ~a~%~a ~a ~a~%" i j k
	      (pentagonal j) (pentagonal k)
	      (- (pentagonal k) (pentagonal j))))))