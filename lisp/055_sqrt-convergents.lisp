(do ((k 1 (1+ k))
     (count 0 count)
     (prev-num 1 num)
     (num 3 (+ (* 2 num) prev-num))
     (prev-denom 1 denom)
     (denom 2 (+ (* 2 denom) prev-denom)))
    ((> k 1000) count)
  (when (> (num-digits num) (num-digits denom))
    (incf count)))