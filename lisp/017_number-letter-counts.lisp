(defvar ones-hundreds-digit-to-str
  '((0 . "zero") (1 . "one") (2 . "two")
    (3 . "three") (4 . "four") (5 . "five")
    (6 . "six") (7 . "seven") (8 . "eight")
    (9 . "nine")))

(defvar ten-to-twenty-to-str
  '((10 . "ten") (11 . "eleven") (12 . "twelve")
    (13 . "thirteen") (14 . "fourteen") (15 . "fifteen")
    (16 . "sixteen") (17 . "seventeen") (18 . "eighteen")
    (19 . "nineteen")))

(defvar tens-digit-to-str
  '((2 . "twenty") (3 . "thirty") (4 . "forty")
    (5 . "fifty") (6 . "sixty") (7 . "seventy")
    (8 . "eighty") (9 . "ninety")))

(defun say-out (n)
  (cond
    ((<= 1 n 9) (cdr (assoc n ones-hundreds-digit-to-str)))
    ((<= 10 n 19) (cdr (assoc n ten-to-twenty-to-str)))
    ((<= 20 n 99) (concatenate 'string
			       (cdr (assoc (floor (/ n 10)) tens-digit-to-str))
			       (say-out (mod n 10))))
    ((<= 100 n 999) (concatenate 'string
				 (cdr (assoc (floor (/ n 100)) ones-hundreds-digit-to-str))
				 "hundred"
				 (if (zerop (mod n 100)) "" "and")
				 (say-out (mod n 100))))
    ((= n 1000) "onethousand")))

(loop with len = 0
      for i from 1 to 1000
      do
	  (incf len (length (say-out i)))
      finally (return len))
