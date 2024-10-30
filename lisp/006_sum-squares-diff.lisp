(- (expt (apply #'+ (loop for i from 1 to 100 collect i)) 2)
   (apply #'+ (loop for i from 1 to 100 collect (* i i))))