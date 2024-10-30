(defmacro avg (a b)
  `(+ (min ,a ,b) (/ (abs (- ,b ,a)) 2)))

(defun binary-search (item seq)
  (unless seq
    (return-from binary-search (values nil 0)))
  (do* ((min 0)
	(max (- (length seq) 1))
	(mid (floor (avg min max))
	     (floor (avg min max)))
	(item2 (elt seq mid)
	       (elt seq mid)))
      (nil)
    (cond
      ((= item item2)
       (return (values t mid)))
      ((< item item2)
       (setq max (- mid 1))
       (when (> min max)
	 (return (values nil mid))))
      ((> item item2)
       (setq min (+ mid 1))
       (when (< max min)
	 (return (values nil (1+ mid))))))))

(defmacro insert-at (idx lst item)
  `(progn
     (if (<= ,idx 0)
	 (push ,item ,lst)
	 (push ,item (cdr (nthcdr (- ,idx 1) ,lst))))
     ,lst))

(defmacro pushnew-in-order (item lst)
  `(multiple-value-bind (found idx) (binary-search ,item ,lst)
     (unless found
       (insert-at idx ,lst ,item))))

(defvar *abundants* '())
(defvar *sums-abundants* '())

(dorange (i 2 28124)
  (when (> (sum-divisors i) i)
    (push i *abundants*)))

(setq *abundants* (reverse *abundants*))

(block top
  (let ((len (length *abundants*)))
    (doranges ((i 0 len) (j i len))
      (let ((xi (elt *abundants* i))
	    (xj (elt *abundants* j)))
	(when (and (zerop (mod i 10))
		   (= j i))
	  (format t "~d ~d~%" i xi))
	(when (> xi 28123/2)
	  (return-from top))
	(when (and (<= (+ xi xj) 28123)
		   (not (member (+ xi xj) *sums-abundants*)))
	  (push (+ xi xj) *sums-abundants*))))))

(block top
  (let ((len (length *abundants*)))
    (doranges ((i 0 len) (j i len))
      (let ((xi (elt *abundants* i))
	    (xj (elt *abundants* j)))
	(when (> xi 28123/2)
	  (return-from top))
	(when (and (zerop (mod i 10))
		  (= j i))
	    (format t "~d~%" i))
	(when (<= (+ xi xj) 28123)
	  (pushnew-in-order (+ xi xj) *sums-abundants*))))))

(loop for i from 1 to 28123
      if (not (find i *sums-abundants*))
	sum i)