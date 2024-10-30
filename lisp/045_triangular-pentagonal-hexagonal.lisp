(loop for i from 2 to 1000000000
      for n = (pentagonal i)
      do (when (and (triangularp n) (hexagonalp n))
	   (format t "~a~%" n)
	   (return)))