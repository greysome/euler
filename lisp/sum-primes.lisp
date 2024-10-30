(+ (loop for i from 3 to 2000000 by 2
	 if (primep i)
	   sum i)
   2)