# IST 772
# HW 9
# MATTHEW PERGOLSKI
# DR. BLOCK
# 12/6/2021

#############################################################################

# install.packages("/Users/pergolicious/OneDrive - Syracuse University/Syracuse University/Courses/IST 772/BaylorEdPsych_0.5.tar.gz", repos = NULL, type = "source")
# library(BaylorEdPsych)
# libarary(car)
# install.packages("MCMCpack")
# library(MCMCpack)

# TThe homework for week 9 is exercises 1, 5, 6 and 7 on page 234.

#############################################################################

# 1. The built-in data sets of R include one called "mtcars," which stands for Motor Trend
# cars. Motor Trend was the name of an automotive magazine and this data set contains
# information on cars from the 1970s. Use "?mtcars" to display help about the data set.
# The data set includes a dichotomous variable called vs, which is coded as 0 for an
# engine with cylinders in a v-shape and 1 for so called "straight" engines. Use logistic
# regression to predict vs, using two metric variables in the data set, gear (number of
#                                                                             forward gears) and hp (horsepower). Interpret the resulting null hypothesis significance
# tests.
    
    #Data
    ?mtcars
    mtcars
    str(mtcars)
    summary(mtcars)
    
    #Model
    mt.glm <- glm(vs ~ gear + hp, family = binomial(link = "logit"), data = mtcars)
    mt.glm
    
    plot(mt.glm)
    summary(mt.glm)
    
    cof <- coef(mt.glm)
    cof
    exp(cof)
    
    #Interpretation
    # From the model generated above, we do not see statistical significance in terms of the 
    # independent variable of 'gear' having a significant effect on the 'vs' variable (i.e., dependent variable).
    # however, it appears as though the other independent variable, hp, does have a p-value below the 
    # threshold at 0.0141, which is under the threshold of 0.05.  Moreover, we can also indicate that
    # the odds are 3.797461e-01:1 for 'gear' and 9.230734e-01 for 'hp.'
    
    
#############################################################################
    
# 5. As noted in the chapter, the BaylorEdPsych add-in package contains a procedure for
# generating pseudo-R-squared values from the output of the gIm() procedure. Use the
# results of Exercise 1 to generate, report, and interpret a Nagelkerke pseudo-R-squared
# value.
    
    PseudoR2(mt.glm)
    
    #Interpretation
      # According to the book, "the  Nagelkerke comes out consistently larger than the others and Smith and McKenna  (2012) suggest that it is the closest analog to the plain, old R-squared that is used  in least-squares models."
      # Although there is much debate in the community over which is best, the Nagelkerke value
      # points to 0.7789526 which indicates significance.  The fact that our data set is small one 
      # suggests that why this result may be more significant than previously thought.
    

#############################################################################
    
# 6. Continue the analysis of the Chile data set described in this chapter. The data set is
# in the "car" package, so you will have to install .packages() and library) that package
# first, and then use the data(Chile) command to get access to the data set. Pay close
# attention to the transformations needed to isolate cases with the Yes and No votes as
# shown in this chapter. Add a new predictor, statusquo, into the model and remove the
# income variable. Your new model specification should be vote ~ age + statusquo.
# The statusquo variable is a rating that each respondent gave indicating whether they
# preferred change or maintaining the status quo. Conduct general linear model and
# Bayesian analysis on this model and report and interpret all relevant results. Compare
# the AIC from this model to the AIC from the model that was developed in the chapter
# (using income and age as predictors).
    
    #data prep
    data(Chile)
    
    chile.df <- data.frame(Chile)
    chile.N <- chile.df[chile.df$vote =='N',]
    chile.Y <- chile.df[chile.df$vote =='Y',]
    chile.YN <- rbind(chile.Y, chile.N)
    chile.YN <- chile.YN[complete.cases(chile.YN),]
    chile.YN$vote <- factor(chile.YN$vote, levels = c('N','Y'))
    
    # conventional model
    chile.lm <- glm(vote ~ age + statusquo, family = binomial(), data = chile.YN)
    summary(chile.lm)
    
    # bayesian model
    chile.YN$vote <- as.numeric(chile.YN$vote) -1
    chile.YN
    str(chile.YN)
    chile.bayesian <- MCMClogit(vote ~ age + statusquo, family = binomial(), data = chile.YN)
    chile.bayesian
    
    summary(chile.bayesian)
    
    # Interpreataion
      # We see that the 'statusquo' variable is significant with a p-value of 2e-16;
      # Furthermore, we see that the HDI of the Bayesian perspective does not span across zero.
      # Becasue of this observation, we can assume it's likely to also be significant.  In summary,
      # it appears as both the conventional and Bayesian tests agree.
    
    
#############################################################################
    
# 7. Bonus R code question: Develop your own custom function that will take the posterior
# distribution of a coefficient from the output object from an MCMClogit() analysis and
# automatically create a histogram of the posterior distributions of the coefficient in terms
# of regular odds (instead of log-odds). Make sure to mark vertical lines on the histogram
# indicating the boundaries of the 95% HDI.
    
    # BONUS QUSETION -- THIS IS OPTIONAL ACCORDING TO PROFESSOR DR. BLOCK.
    
    
    



