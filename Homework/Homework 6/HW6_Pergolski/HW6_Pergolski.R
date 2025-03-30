# IST 772
# HW 6
# MATTHEW PERGOLSKI
# DR. BLOCK
# 11/11/2021

#############################################################################

# The homework for week 6 is exercises 1-7 on pages 117 and 118.
    
#############################################################################

# 1. The data sets package (installed in R by default) contains a data set called InsectSprays
  # that shows the results of an experiment with six different kinds of insecticide. For each
  # kind of insecticide, n = 12 observations were conducted. Each observation repre-
  #  sented the count of insects killed by the spray. In this experiment, what is the depen-
  #  dent variable (outcome) and what is the independent variable? What is the total number
  # of observations?
      
      head(InsectSprays)
      # Dependent variable would be the amount of insects killed and the independent variable(s)
        # would be the different types, or categories, of spray; total number of observations is 72.
      
      
            
#############################################################################

# 2. After running the aov() procedure on the InsectSprays data set, the "Mean Sq" for spray
  # is 533.8 and the "Mean Sq" for Residuals is 15.4. Which one of these is the between-
  #  groups variance and which one is the within-groups variance? Explain your answers
  # briefly in your own words.


      insectResults <- aov(count ~ spray, data = InsectSprays)
      summary(insectResults)
      
      # The 533.8 value corresponds to the 'Between Groups' variance while the 15.4 value corresponds to
      # the 'Within Groups' variance.  The ratio of Between Groups divided by Within Groups variance
      # gives us our F-value -- which, in this case, is 34.7 (533.8 / 15.4).


#############################################################################

# 3. Based on the information in question 2 and your response to that question, calculate an
  # F-ratio by hand or using a calculator. Given everything you have earned about F-ratios,
  # what do you think of this one? Hint: If you had all the information you needed for a Null
  # Hypothesis Significance Test, would you reject the null? Why or why not?
      
      # F-ratio calculated: 533.8 / 15.4 = 34.7
      # Based on the conventional rules of the F-value, it would generally equal ~1 if there were no real,
      # significant differences between the groups; however, if we spot outliers, or values that are far
      # greater than 1, it gives us evidence to suggest that a significant difference is real, and thus
      # we'd be inclined to reject the null hypothesis, provided we could confirm that our p-value is 
      # less than the specified alpha value (which may be 0.05 in many traditional academic cases).
        
#############################################################################

# 4. Continuing with the InsectSprays example, there are six groups where each one has n
  # = 12 observations. Calculate the degrees of freedom between groups and the degrees
  # of freedom within groups. Explain why the sum of these two values adds up to one less
  # than the total number of observations in the data set.
      
      summary(insectResults)
      
      # Within our summary(insectResults) command,  we can spot the 'DF' section that indicates our
      # degrees of freedom.  If we add up the residuals value of 66 and spray value of 5, we get one less
      # than the total number of observations within the group.
      
      # As we perform calculations on a data set, we are essentially using up information that leaves us with less
      # moving forward.  This reduction in information/data increases our uncertainty, and as a result we need to subtract
      # in order to have an accurate approach to finding the most unbiased statistic/end-value. An example would be calculating the mean 
      # when attempting to find the variance -- this intermediate step of calculating the mean would require us
      # to subtract one (1).
      
#############################################################################

# 5. Use R or R-Studio to run the aov() command on the InsectSprays data set. You will
  # have to specify the model correctly using the "~" character to separate the dependent
  # variable from the independent variable. Place the results of the aov() command into a
  # new object called insectResults. Run the summary() command on insectResults and
  # interpret the results briefly in your own words. As a matter of good practice, you should
  # state the null hypothesis, the alternative hypothesis, and what the results of the null
  # hypothesis significance test lead you to conclude.
      

    insectResults
    summary(insectResults)
    
    # Interpretation
      # The null hypothesis in this situation suggests that there are is no significant difference between
      # the groups/sprays being compared.  On the opposite end, our alternative hypothesis suggests that there
      # is indeed a significant difference between the various types of sprays within the data set.
      # Based on the results of the frequentest aov() R command, we see that the F-value is significantly
      # larger than one (1). If the variance from the differing groups were about the same, we would expect
      # an F-value of about 1.
      # This result along with the fact that our P value is significantly under 0.05,
      # we can indicate that we reject the null-hypothesis and conclude there may be evidence to suggest
      # a significant difference exists between our independent variables.



#############################################################################

# 6. Load the BayesFactor package and run the anovaBF() command on the InsectSprays
  # data set. You will have to specify the model correctly using the "~" character to sepa-
  #   rate the dependent variable from the independent variable. Produce posterior distribu-
  #   tions with the posterior() command and display the resulting DIs. Interpret the results
  # briefly in your own words, including an interpretation of the BayesFactor produced by the grouping variable. As a matter of good practice, you should state the two hypoth-
  #   eses that are being compared. Using the rules of thumb offered by Kass and Raftery
  # (1995), what is the strength of this result?
      
    # background info for context
    # library(BayesFactor)
    IS <- InsectSprays
    str(InsectSprays)
    
    # Bayesian ANOVA
    SpraysBayesOut <- anovaBF(count ~ spray, data = InsectSprays)
    SpraysBayesOut
    str(SpraysBayesOut)
    
    # MCMC method on Bayesian ANOVA with plots
    SpraysMCMCout <- posterior(SpraysBayesOut, iterations = 10000)
    plot(SpraysMCMCout[,"mu"])
    summary(SpraysMCMCout)Ye
    boxplot(as.matrix(SpraysMCMCout[,2:7]), las = 2)
    
    # Interpretation
    # Our null hypothesis suggests that there is no significant difference in the effectiveness of the different
      # sprays (i.e., independent variables) within our data set.  The alternative hypothesis indicates the
      # opposite (i.e., there is a significant difference between the variables).  When conducting the Bayesian
      # ANOVA test, we see that the spray variable has a 1.506706e+14 ±0% to 1 ratio supporting the alternative
      # hypothesis, which is exceptional odds when validating against Kass and Raftery's rule of thumb 
      # (i.e., odds ratios of more than 150:1 are very strong evidences for the favored hypothesis).
      # when visualizing the HDIs in boxplot form, we see a difference in the measured effectiveness 
      # of the various sprays; Spray C seems to be the worst performer while Spray F appears to be the 
      # best
    
#############################################################################

#   7. In situations where the alternative hypothesis for an ANOVA is supported and there
  # are more than two groups, it is possible to do post-hoc testing to uncover which pairs
  # of groups are substantially different from one another. Using the InsectSprays data,
  # conduct a t-test to compare groups C and F (preferably a Bayesian t-test). Interpret the
  # results of this t-test.
    
    # Frequentest Perspective
    spray.c <- InsectSprays$count[InsectSprays$spray == "C"]
    spray.c
    spray.f <- InsectSprays$count[InsectSprays$spray == "F"]
    spray.f
    t.test(spray.c, spray.f)
    
    # Bayesian Perspective
    # library(BEST)
    BESTmcmc(spray.c, spray.f)
    plot(BESTmcmc(spray.c, spray.f))      
    
    # Interpretation
    # Within the t.test, from a Bayesian perspective, we can see that 100% of the data is showing
    # on the negative section of the graph, with an HDI spanning from -18.9 to -10.1.  This indicates that
    # we can expect, with 95% confidence, that the difference between spray C and F is within this range,
    # with the most likely difference being the point-estimate, or -14.5.  This would suggest that spray
    # C has a disadvantage (i.e., negative performance) of 14.5 units compared to spray F; if we also compare this to the frequentest perspective, we observe
    # a p-value significantly less than the conventional alpha value of 0.05 -- so 
    # we can reject the null hypothesis, which assumed there was no difference between the two variables.
      
      
      
    
    




































