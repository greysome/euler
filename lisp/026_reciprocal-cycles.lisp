(defun quotient-digit (a b)
  "Let k be the smallest number such that 10^k * a > b.
Return the quotient and remainder of 10^k*a / b, as well as k.
e.g. (quotient-digit 3 7) => 4 1

Assumes that a < b."
  (let ((k 0))
    (do ()
	((>= (* (expt 10 k) a) b))
      (incf k))
    (let ((a1 (* (expt 10 k) a)))
      (values (floor (/ a1 b))
	      (mod a1 b)
	      k))))

(defun append-with-zeros (lst1 n lst2)
  (let ((tmp lst1))
    (dotimes (i n)
      (setq tmp (append tmp '(0))))
    (append tmp lst2)))

(defun get-recurring-decimals (a b)
  "Return the recurring portion of the decimal expansion of a/b,
or NIL if it doesn't recur."
  (do* ((cur-rem a)
        (expansion '())
	(prev-rems '()))
       (nil)
    (multiple-value-bind (quot rem k)
	(quotient-digit cur-rem b)
      ;;(format t "~a ~a~%" prev-rems expansion)
      ;;(format t "~d/~d: ~d = ~d*~d + ~d~%" cur-rem b
      ;;	      (* (expt 10 k) cur-rem) quot b rem)
      (when (zerop rem)
	(return-from get-recurring-decimals '()))
      (when (find rem prev-rems)
	(return-from get-recurring-decimals expansion))
      (setq cur-rem rem)
      (setq prev-rems (append prev-rems (list rem)))
      (setq expansion (append-with-zeros expansion (- k 1) (list quot))))))

(let ((max-recurring-len 0)
      (d 0)
      (d-recurring '()))
  (dorange (i 1 1000)
    (let* ((recurring (get-recurring-decimals 1 i))
	   (len (length recurring)))
      (when (> len max-recurring-len)
	(setq max-recurring-len len
	      d i
	      d-recurring recurring))))
  (values max-recurring-len d d-recurring))