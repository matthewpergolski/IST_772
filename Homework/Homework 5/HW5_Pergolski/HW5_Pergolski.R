# IST 772
# Dr. Block
# Matthew L. Pergolski

# The homework for week five is exercises 6 through 10 on pages 86 and 87.

################################################

# 6. The PlantGrowth data set contains three different groups, with each representing
    #various plant food diets (you may need to type data(PlantGrowth) to activate it). The
    #group labeled "ctrl" is the control group, while "trt1" and "trt2" are different types of
    #experimental treatment. As a reminder, this subsetting statement accesses the weight
    #data for the control group:
          
    #PlantGrowth$weight[PlantGrowth$group=="ctrl"]
        
    #and this subsetting statement accesses the weight data for treatment group 1:
          
    #PlantGrowth$weight[PlantGrowth$group=="trt1"]
        
    #Run a t-test to compare the means of the control group ("ctrl") and treatment group
    #1 ("trt1") in the PlantGrowth data. Report the observed value of t, the degrees of
    #freedom, and the p-value associated with the observed value. Assuming an alpha
    #threshold of .05, decide whether you should reject the null hypothesis or fail to reject
    #the null hypothesis. In addition, report the upper and lower bound of the confidence
    #interval.
    
    #data
    data(PlantGrowth)
    
    #subset - control group
    ctrl <- PlantGrowth$weight[PlantGrowth$group=="ctrl"]
    ctrl

    #subset - treatment group 1
    trt1 <- PlantGrowth$weight[PlantGrowth$group=="trt1"]
    trt1
    
    #t.test
    t.test(ctrl, trt1)
    # The observed value of t comes out to be 1.1913 while the degrees of freedom are 16.524; the p-value equates to 0.2504.
    # Based on our alpha value of 0.05, we fail to reject the null hypothesis that there is no difference between the two groups, since our p-value (0.2504) is greater than our alpha value (0.05).
    # The upper and lower bound of the confidence interval is the following: -0.2875162 through 1.0295162.
    
    ################################################
    
# 7. Install and library() the BEST package. Note that you may need to install a program
    #called JAGS onto your computer before you try to install the BEST package inside of
    #R. Use BESTmcmc() to compare the PlantGrowth control group ("ctrl") to treatment
    #group 1 ("trt1"). Plot the result and document the boundary values that BESTmcmc()
    #calculated for the HD. Write a brief definition of the meaning of the HDI and interpret
    #the results from this comparison.
    
    install.packages("BEST")
    library(BEST)
    
    # BEST MCMC and plot
    bayesian <- BESTmcmc(ctrl, trt1)
    bayesian
    
    plot(bayesian)
    # The HDI boundary values are -0.357 and 1.15.  
    # The HDI conveys to the reader that the population mean has a 95% probability of existing between the interval that is shown (-0.357 through 1.15).  The point estimate, or 0.384, is our most likely candidate value for the population mean.
    # We can see that, from the graph, that about 14.5 percent of values from this distribution are shown to be on the left hand side of the zero value (i.e., negative) while 85.5% of the values are in the positive realm.
    
    ################################################

# 8. Compare and contrast the results of Exercise 6 and Exercise 7. You have three types
    #of evidence: the results of the null hypothesis test, the confidence interval, and the
    #HDI from the BESTmcmc() procedure. Each one adds something, in turn, to the
    #understanding of the difference between groups. Explain what information each test
    #provides about the comparison of the control group ("ctrl") and the treatment group 1
    #("trt1").
    
    # See PDF file submitted with homework 5
    
    ################################################
    
# 9. Using the same PlantGrowth data set, compare the "ctrl" group to the "trt2" group.
    #Use all of the methods described earlier (t-test, confidence interval, and Bayesian
    #method) and explain all of the results.
    
    #ctrl group
    ctrl
    
    #trt2 group
    trt2 <- PlantGrowth$weight[PlantGrowth$group=="trt2"]
    trt2
    
    #t.test
    t.test(ctrl, trt2)
    # We have a t value of t = -2.134 and a degrees of freedom value corresponding to df = 16.786; we also have a p-value of 0.0479, which is smaller -- barely -- than the alpha value of 0.05, meaning that our results are statistically significant and we can reject the null hypothesis that there is no difference between the two groups.
    # our confidence interval has a range of -0.98287213 and -0.00512787, meaning that -- over the long run -- our population mean would be estimated to correspond to a place within this range 95 out of 100 times.
    
    #bayesian method
    BESTmcmc(ctrl, trt2)
    plot(BESTmcmc(ctrl, trt2))
    # Within our distribution, can conclude, with 95% confidence, that the true population mean difference lies within the interval range of -1.04 and 0.0755.  From this, we see that 95.9% of the data within the dist. is negative, and that the most likely mean equates to -.0487.
    
    ################################################
    
# 10. Consider this t-test, which compares two groups of n = 100,000 observations each:
      
    #t.test(rnorm(100000,mean=17.1,sd=3.8),rnorm(100000,mean=17.2,sd=3.8))
    
    #For each of the groups, the norm () command was used to generate a random normal
    #distribution of observations similar to those for the automatic transmission group in
    #the mtcars database (compare the programmed standard deviation for the random
    #normal data to the actual mtcars data). The only difference between the two groups
    #is that in the first norm() call, the mean is set to 17.1 mpg and in the second it is set to
    #17.2 mpg. think you would agree that this is a negligible difference, if we are discuss-
    #ing fuel economy. Run this line of code and comment on the results of the t-test. What
    #are the implications in terms of using the NHST on very large data sets?
    
    t.test(rnorm(100000,mean=17.1,sd=3.8),rnorm(100000,mean=17.2,sd=3.8))
    
    # We seem to have a t value of t = -5.8156, degrees of freedom of df = 2e+05, and p-value of p-value = 6.05e-09 (meaning that our resutls are statistically significant and we reject the null hypothesis).
    # We have a confidence interval of -0.13244696 -0.06567583, which is a relatively small range.
    # We can conclude that, with large datasets, we will see statistically significant results for even the smallest of differences between the data will cause the t.tester to reject the null hypothesis that the two groups have no (meaningful) difference, which can be seen as unreliable.
    
    
    
    
    
    
    
    
    