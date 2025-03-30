set.seed(5)
toastAngleData <- runif(1000,0,180)
round(mean(toastAngleData))



# When we repeatedly draw samples from a population and calculate the mean of each of those samples, we
#can provide a picture of the long-term trends of sampling. We are, in effect, creating a new data set, one
#consisting solely of sample means. By studying that collection of sample means, we will be able to reason
#about future samples.


quantile(0:100,probs=0.75)
?qnorm()
qnorm(0.50)
qnorm(0.75)
qnorm(0.25)
qnorm(0.975)





mean(replicate(10000,mean(sample(0:100, size=3))))




