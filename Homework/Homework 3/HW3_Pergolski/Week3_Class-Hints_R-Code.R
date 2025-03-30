

# breakout 1
set. seed(1234) Control randomization
testPop <- rnorm (100000, mean=100, sd=10)# Make simulated pop

hist(testPop)
abline(v=quantile(testPop, probs=c(0.25,0.5,0.75)), col="blue")

testPopMean <- mean (testPop) # Calculate mean
sumsq <- sum((testPop-testPopMean)^2)
testPopVar sumsq/length(testPop) # Sum of squares
# Pop Variance
testPopSD sqrt(testPopVar) Pop Std Deviation
abline(v=(testPopMean - 2*testPopSD), col="red")
abline(v=(testPopMean + 2* testPopSD), col="red")
          
abline(v=quantile(testPop, probs=0.025), col="green") #Lower tail
abline(v=quantile(testPop, probs=0.975), col="green") # Upper tail


#breakout 2
set.seed(1234) Control randomization

testPop <- norm(100000, mean=100, sd=10) # Create simulated pop

# Custom function to pull one sample of size n I

sampleTestScores <- function (n) 
  {
  
  sample (testPop, size=n, replace=TRUE)
  
  }

sampleTestScores(100)

# write a comment explaining this line

samplingDistribution <- replicate(1000, mean (sampleTestscores(100)))

# par (mfrow=c(2,1))
hist (testPop, xlim=c(50,140))
hist (samplingDistribution, xlim=c(50,140))
par (mfrow=c(1,1))

toastAngleData <- runif(1000,0,180) # Random numbers from uniform distribution
head(toastAngleData)
tail(toastAngleData)
mean(toastAngleData)
hist(toastAngleData)

# uniform distiribution = all observations are close to the same value: runif()
# normal distribution = ?: rnorm()
# principles are the same between normal and uniform dist; central limit theorem and 
    # law of large numbers
















