mtcars

mean( mtcars$mpg[mtcars$am == 0] ) # Automatic transmissions
mean( mtcars$mpg[mtcars$am == 1]) # Manual transmissions

sd( mtcars$mpg[ mtcars$am == 0 ] ) # Automatic transmissions
sd( mtcars$mpg[ mtcars$am == 1 ] ) # Manual transmissions

boxplot(mpg ~ am, data=mtcars) # Boxplot of mpg, grouped by am

mean( sample(mtcars$mpg[ mtcars$am == 0 ], size=19 ,replace=TRUE) )
mean( sample(mtcars$mpg[ mtcars$am == 1 ],size=13, replace=TRUE))

mean(sample(mtcars$mpg[mtcars$am == 0],size=19,replace=TRUE)) -
  mean(sample( mtcars$mpg[mtcars$am == 1],size=13,replace=TRUE) )

meanDiffs <- replicate(100,mean(sample(mtcars$mpg[mtcars$am == 0 ], size= 19, replace=TRUE) ) - mean(sample(mtcars$mpg[mtcars$am == 1 ], size=13, replace=TRUE)))
meanDiffs

hist(meanDiffs)

quantile(meanDiffs, c(0.025, 0.975))


t.test(mtcars$mpg[mtcars$am==0],mtcars$mpg[mtcars$am==1])

library(animation)
conf.int(level=0.95)


