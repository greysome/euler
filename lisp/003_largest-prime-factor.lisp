(let ((n 600851475143) (max-factor 2))
  (dorange (i 2 (isqrt n))
    (when (and (primep i) (zerop (mod n i)))
      (setf max-factor i)))
  max-factor)

(let ((n 600851475143) (max-factor 2))
  (defun recurse (k)
    (dorange (i 2 (1+ (isqrt k)))
      (when (zerop (mod k i))
	(when (> i max-factor)
	  (setf max-factor i))
	(recurse (/ k i))
	(return-from recurse)))
    ;; k is prime
    (when (> k max-factor)
      (setf max-factor k)))
  (recurse n)
  max-factor)