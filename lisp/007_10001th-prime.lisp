(do ((counter 3 (incf counter 2))
     (last-prime 2)
     (prime-idx 1))
    ((= prime-idx 10001) last-prime)
  (when (primep counter)
    (setq last-prime counter)
    (incf prime-idx)))