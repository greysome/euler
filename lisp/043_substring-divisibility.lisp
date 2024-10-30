(defun prop (digit-list)
  (and
   (divisiblep (from-digits (subseq digit-list 1 4)) 2)
   (divisiblep (from-digits (subseq digit-list 2 5)) 3)
   (divisiblep (from-digits (subseq digit-list 3 6)) 5)
   (divisiblep (from-digits (subseq digit-list 4 7)) 7)
   (divisiblep (from-digits (subseq digit-list 5 8)) 11)
   (divisiblep (from-digits (subseq digit-list 6 9)) 13)
   (divisiblep (from-digits (subseq digit-list 7 10)) 17)))

(loop for digit-list in (permutations (nreverse (range 0 10)))
      when (prop digit-list)
	sum (from-digits digit-list))
