install.packages("datasets")
library(datasets)
set.seed(1)
pgrp1 <- sample(precip,20, replace=TRUE)
pgrp2 <- sample(precip,20, replace=TRUE)
pgrp3 <- sample(precip,20, replace=TRUE)



var(c(pgrp1,pgrp2,pgrp3)) # Join all of our sample data and calc variance

var(precip)

mean(pgrp1) # Examine the means of the three groups
mean(pgrp2)
mean(pgrp3)
# Create a bar plot of the means
barplot(c(mean(pgrp1),mean(pgrp2),mean(pgrp3)))
var(c(mean(pgrp1),mean(pgrp2),mean(pgrp3))) # Variance among the means

# Take away five inches of rain from each point in sample 3
pgrp3 <- pgrp3 - 5
# Bar plot of the new means
barplot(c(mean(pgrp1),mean(pgrp2),mean(pgrp3)))

var(c(mean(pgrp1),mean(pgrp2),mean(pgrp3)))

var(c(pgrp1,pgrp2,pgrp3))

randomFs <- rf(n=100,df1=2,df2=57)
hist(randomFs)


# Run ANOVA on groups sampled from the same population
set.seed(10) # Control the randomization
# Enough for 3 groups of 20
precipAmount <- sample(precip,60, replace=TRUE)
precipAmount
# Group designators, 3 groups
precipGrp<-as.factor(rep(seq(from=1,to=3,by=1),20))
precipGrp
# Put everything in data frame
precipDF <- data.frame(precipAmount, precipGrp)
precipDF
# Get a box plot of the distribs
boxplot(precipAmount ~ precipGrp, data=precipDF)
# Run the ANOVA
precipOut <- aov(precipAmount ~ precipGrp, data=precipDF)
precipOut
summary(precipOut) # Provide an ANOVA table




install.packages('gtools') # install gtools to get permutations
library(gtools) # Make the package ready
tinyPop <- c(1,2,3) # Here is a tiny population
# This next command gives every possible sample with replacement!
allSamp <- permutations(n=3,r=3,v= tinyPop, repeats.allowed=T)
allSamp # Verify: 27 unique samples
apply(allSamp, 1, var) # List sample variance of each sample
mean(apply(allSamp,1,var)) # What is the mean of those variances?

fVals <- seq(from=0.01,to=5,by=0.01) # Make a sequential list of F values
fVals
# Plot the density function for this scenario
plot(fVals, df(fVals,df1=2,df2=57))
# Add points to the same plot, different df1
points(fVals, df(fVals,df1=3,df2=57))
# Add points to the same plot, different df1
points(fVals, df(fVals,df1=4,df2=57))



install.packages("BayesFactor")
library(BayesFactor)


#install.packages "BEST") # May not need this if BEST is downloaded
library(BEST) # Likewise, may not need this
data(mtcars) # Makes mtcars ready for analysis
priorList <- list(muM = c(20,20), muSD = c(4,4)) # 1 mean, T SD for each group
carsBest2 <- BESTmcmc(mtcars$mpg[mtcars$am==0],
                      mtcars$mpg[mtcars$am==1], priors=priorList)
plot(carsBest2) # Review the HDI and other results

precipBayesOut <- anovaBF(precipAmount ~ precipGrp, data=precipDF)
precipBayesOut
mcmcOut <- posterior(precipBayesOut, iterations=10000) # Run MCMC iterations
mcmcOut
plot(mcmcOut[,"mu"]) # Show the range of values for the grand mean


par(mfcol=c(1,1)) # Reset display to one column if needed
hist(mcmcOut[,"precipGrp-1"]) # Histogram of MCMC results
# Mark the upper and lower boundaries of the 95% HD with vertical lines
abline(v=quantile(mcmcOut[,"precipGrp-1"],c(0.025)),col="black")
abline(v=quantile(mcmcOut[,"precipGrp-1"],c(0.975)), col="black")
# Give numeric value of lower bound
quantile(mcmcOut[,"precipGrp-1"],c(0.025))
# Give numeric value of upper bound
quantile(mcmcOut[,"precipGrp-1"],c(0.975))

boxplot(as.matrix(mcmcOut[,2:4]))
summary(mcmcOut)
precipBayesOut








data(chickwts) # Probably not needed
str(chickwts)
summary(chickwts)
chicksOut<- aov(weight ~ feed, data=chickwts) # Run the ANOVA
chicksOut
summary(chicksOut) # Show the ANOVA table


# Calc Bayes Factors
chicksBayesOut<- anovaBF(weight ~ feed, data=chickwts)
chicksBayesOut
# Run mcmc iterations
mcmcOut2 <- posterior(chicksBayesOut, iterations=10000)
mcmcOut2
boxplot(as.matrix(mcmcOut2[,2:7])) # Boxplot the posteriors for the groups
summary(mcmcOut2) # Show the HDIs



plot(BESTmcmc(chickwts[chickwts$feed=="sunflower",1],
              chickwts[chickwts$feed=="meatmeal",1]))

chicksBayesOut









