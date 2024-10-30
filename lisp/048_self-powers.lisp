(mod (loop for i from 1 to 1000
	   sum (expt-mod i i 10000000000))
     10000000000)
