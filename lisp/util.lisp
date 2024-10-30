(defpackage #:project-euler
  (:use #:cl #:cl-utilities))

(in-package :project-euler)

(defmacro print2 (&rest things)
  `(print (list ,@things)))

;; CONTROL STRUCTURES
(defun range (start end &optional (step 1))
  (loop for i from start to (dec end) by step
	collect i))

(defmacro dorange ((var start end &optional (step 1)) &body body)
  (once-only (step end)
    `(do ((,var ,start (+ ,var ,step)))
	 ((funcall (if (> ,step 0) #'>= #'<=) ,var ,end))
       ,@body)))

(defmacro doranges (ranges &body body)
  (if ranges
      `(dorange ,(car ranges)
	 (doranges ,(cdr ranges) ,@body))
      `(progn ,@body)))

(defmacro doenum ((var idx-var lst) &body body)
  "An analogue of `for idx-var, var in enumerate(lst)` in Python."
  `(dorange (,idx-var 0 (length ,lst))
     (let ((,var (elt ,lst ,idx-var)))
       ,@body)))

;;; DIGITS-RELATED
(defun num-digits (n &optional (base 10))
  (+ (floor (log n base)) 1))

(defun digit (n k &optional (base 10))
  (mod (floor (/ n (expt base k)))
   base))

(defun digits (n &optional (base 10))
  (if (zerop n)
  '(0)
  (loop with d = (dec (num-digits n base))
	for i from 0 to d
	collect (digit n (- d i) base))))

(defun from-digits (digits &optional (base 10))
  (loop with d = (dec (length digits))
    with exponent = 0
    for i in digits
    sum (* i (expt base (- d exponent)))
    do (incf exponent)))

(defun sum-digits (n &optional (base 10))
  (loop for i from 0 to (- (num-digits n base) 1)
    sum (digit n i base)))

(defun palindromep (n &optional (base 10))
  (let ((k (dec (num-digits n base))))
    (dorange (i 0 (ceiling (/ k 2)))
      (unless (= (digit n i base)
		 (digit n (- k i) base))
	(return-from palindromep nil))))
  t)

(defun reverse-num (n &optional (base 10))
  (from-digits (nreverse (digits n base))))

(defun same-elements (lst1 lst2)
  (and (subsetp lst1 lst2) (subsetp lst2 lst1)))

(defun pandigitalp (n &optional (digit-list (range 1 10)))
  (same-elements (uniques (digits n))
		 digit-list))

;;; MATH-RELATED
(defun dec (n)
  (- n 1))

(defmacro divisiblep (n k)
  `(zerop (mod ,n ,k)))

(defun squarep (n)
  (= (expt (isqrt n) 2) n))

(defun triangular (n)
  (/ (* n (1+ n)) 2))

(defun triangularp (n)
  (and
   (squarep (1+ (* 8 n)))
   (evenp (dec (isqrt (1+ (* 8 n)))))))

(defun pentagonal (n)
  (/ (* n (dec (* 3 n))) 2))

(defun pentagonalp (n)
  (and
   (squarep (1+ (* 24 n)))
   (divisiblep (1+ (isqrt (1+ (* 24 n)))) 6)))

(defun hexagonal (n)
  (* n (dec (* 2 n))))

(defun hexagonalp (n)
  (and
   (squarep (1+ (* 8 n)))
   (divisiblep (1+ (isqrt (1+ (* 8 n)))) 4)))

(defun prime-sieve (n)
  "Construct a prime sieve, as an array of {t, nil}, up to `n`."
  (let ((arr (make-array (1+ n) :initial-element t)))
    (dorange (i 2 (isqrt n))
      (when (elt arr i)
	(dorange (j (* 2 i) n i)
	  (setf (elt arr j) nil))))
    arr))

(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(defun nCr (n r)
  (/ (factorial n)
     (* (factorial r) (factorial (- n r)))))

(defun primep (n)
  (when (= n 1)
    (return-from primep nil))
  (dorange (i 2 (1+ (isqrt n)))
    (when (zerop (mod n i))
      (return-from primep nil)))
  t)

(defun primep-or-factor (n)
  "Variant of PRIMEP that returns T if N is prime, and the smallest
  factor otherwise."
  (dorange (i 2 (1+ (isqrt n)))
    (when (zerop (mod n i))
      (return-from primep-or-factor i)))
  t)

(defun factorize (n)
  "Return an association list of (PRIME MULTIPLICITY) where each PRIME
  divides N with multiplicity MULTIPLICITY."
  (let ((factor (primep-or-factor n)))
    (when (eq factor t)
      (return-from factorize (acons n 1 nil)))
    (let* ((recurse-factors (factorize (/ n factor)))
	   (d (assoc factor recurse-factors)))
      (when d
	(rplacd d (1+ (cdr d)))
	(return-from factorize recurse-factors))
      (setq recurse-factors (acons factor 1 recurse-factors))
      (return-from factorize recurse-factors))))

(defun sum-divisors (n)
  (loop with prod = 1
	for (prime . multiplicity) in (factorize n)
	for i = (/ (- (expt prime (1+ multiplicity)) 1)
		   (- prime 1))
	do (setq prod (* prod i))
	finally (return (- prod n))))

(defun num-divisors (n)
  (loop with prod = 1
	for (prime . multiplicity) in (factorize n)
	do
	   (setf prod (* prod (1+ multiplicity)))
	finally (return prod)))


;; MISC
(defun curry (fn &rest args)
  (lambda (&rest remaining-args)
    (apply fn (append args remaining-args))))

(defun arefs (array subscripts-list)
  (mapcar (lambda (subscripts) (apply (curry #'aref array) subscripts))
	  subscripts-list))

(defun arefs-offsets (array cur-subscript offsets-list)
  (arefs array
	 (mapcar (lambda (offsets)
		   (mapcar #'+ cur-subscript offsets))
		 offsets-list)))

(defun uniques (lst)
  (let ((unique-elements '()))
    (mapc (lambda (x) (pushnew x unique-elements)) lst)
    unique-elements))

(defun uniquep (lst)
  (= (length lst) (length (uniques lst))))

(defun remove-idx (idx lst)
  (append (subseq lst 0 idx) (subseq lst (1+ idx))))

(defun permutations (lst)
  (let ((len (length lst)))
    (if (or (= len 0)
	    (= len 1))
	(list lst)
	(mapcan
	 (lambda (x)
	   (mapcar (lambda (perm) (append (list (nth x lst)) perm))
		   (permutations (remove-idx x lst))))
	 (range 0 len)))))