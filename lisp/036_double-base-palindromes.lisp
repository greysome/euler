(loop for i from 1 to 1000000
      if (and (palindromep i 10)
	      (palindromep i 2))
	sum i)