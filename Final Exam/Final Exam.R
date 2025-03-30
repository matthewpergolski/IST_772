# IST 772
# FINAL EXAM
# MATTHEW PERGOLSKI
# DR. BLOCK
# 12/16/2021

############################################

load("~/OneDrive - Syracuse University/Syracuse University/Courses/IST 772/Final Exam/Final Exam Data/allSchoolsReportStatus.RData")
load("~/OneDrive - Syracuse University/Syracuse University/Courses/IST 772/Final Exam/Final Exam Data/districts16.RData")
load("~/OneDrive - Syracuse University/Syracuse University/Courses/IST 772/Final Exam/Final Exam Data/usVaccines.RData")

library(changepoint)


############################################
str(allSchoolsReportStatus)
str(usVaccines)
usVaccines.df <- data.frame(usVaccines)
str(usVaccines.df)
str(districts)

############################################

# 1. How have U.S. vaccination rates varied over time? Are vaccination rates increasing or
# decreasing? Which vaccination has the highest rate at the conclusion of the time series?
#   Which vaccination has the lowest rate at the conclusion of the time series? Which
# vaccine has the greatest volatility?
  
  # In general, without differencing the data (i.e., applying the diff() command), we can see a general increase
  # trend in all attributes that associate with the 'usVaccines' data.  We can confirm this through the plot()
  # command as well as the boxplot() command as well.  Although the beginning of the time period observes
  # volatility, all vaccines appear to end at higher values compared to the beginning of the time series.
  #  At the conclusion of the time series, we can see which vaccine(s) have the highest and lowest value by using
  # the tail() commands.  The highest at the end of the interval appears to be 
  # 'DTP1' with a rate value of 98 while 'HepB_BD' is by far the lowest with a rate value of just 64.
  # With visually looking at the box plots for these vaccine rates, we see the 'HepB_BD' also has the 'longer' spread
  # of values, which suggests a more substantial volatility compared to the other
  # variables.
  
  head(usVaccines)
  tail(usVaccines)
  plot(usVaccines)
  plot.ts(usVaccines)
  boxplot(usVaccines)
  summary(usVaccines)
  
  aov(usVaccines.df)
  
  tail(diff(usVaccines))
  plot(diff(usVaccines))
  boxplot(diff(usVaccines))
  
  cpt.var(usVaccines[,"DTP1"])
  diff(usVaccines[,"DTP1"])
  plot(diff(usVaccines[,"DTP1"]))
  cpt.var(diff(usVaccines[,"DTP1"]))
  
  install.packages("tseries")
  library(tseries)
  plot(diff(usVaccines))
  adf.test(diff(usVaccines[,"DTP1"]))
  adf.test(diff(usVaccines[,"HepB_BD"]))
  adf.test(diff(usVaccines[,"Pol3"]))
  adf.test(diff(usVaccines[,"Hib3"]))
  adf.test(diff(usVaccines[,"MCV1"]))
  
  cor(diff(usVaccines))
  
  colnames(usVaccines)
  # usVaccines.df$DTP1
  # plot(usVaccines.df$DTP1)
  # plot(usVaccines.df$HepB_BD)
  # plot(usVaccines.df$Pol3)
  # plot(usVaccines.df$Hib3)
  # plot(usVaccines.df$MCV1)
  
  # install.packages('ez')
  # library(ez)
  # ezANOVA(usVaccines)
  
  # By visually looking at the above plots, 
  
  View(usVaccines)
  ?aov
  plot(aov(DTP1 ~ ., usVaccines))
  summary(aov(DTP1 ~ ., usVaccines))
  
  ############################################

# 2. What proportion of public schools reported vaccination data? What proportion of
# private schools reported vaccination data? Was there any credible difference in overall
# reporting proportions between public and private schools?
  
  # data prep
    allSchoolsReportStatus
    head(allSchoolsReportStatus)
    str(allSchoolsReportStatus)
    allSchoolsReportStatus$name <- as.factor(allSchoolsReportStatus$name)
    allSchoolsReportStatus$pubpriv <- as.factor(allSchoolsReportStatus$pubpriv)
    allSchoolsReportStatus$reported <- as.factor(allSchoolsReportStatus$reported)
    str(allSchoolsReportStatus)
  
  # proportion of public schools who reported vaccination data
    public.schools <- allSchoolsReportStatus[allSchoolsReportStatus$pubpriv == 'PUBLIC',]
    public.schools
    str(public.schools)
    public.schools.Y <- public.schools[public.schools$reported == 'Y',]
    public.schools.Y
    public.schools.N <- public.schools[public.schools$reported == 'N',]
    public.schools.N
    
    pub.vax.proportion <- length(public.schools.Y$reported) / length(public.schools$reported)
    pub.vax.proportion
    
    # 0.97418 (~97%) of public schools reported vaccinations
  
  
  # proportion of private schools who reported vaccination data
    private.schools <- allSchoolsReportStatus[allSchoolsReportStatus$pubpriv == 'PRIVATE',]
    private.schools
    str(private.schools)
    private.schools.Y <- private.schools[private.schools$reported == 'Y',]
    private.schools.Y
    private.schools.N <- private.schools[private.schools$reported == 'N',]
    private.schools.N
    
    length(private.schools.Y$reported)
    length(private.schools$reported)
    
    pri.vax.proportion <- length(private.schools.Y$reported) / length(private.schools$reported)
    pri.vax.proportion
    
    # 0.8471801 (~85%) of private schools reported vaccinations
    
  # t.test
    public.schools$number <- as.numeric( factor(public.schools$reported) ) -1
    public.schools$number 
    private.schools$number <- as.numeric( factor(private.schools$reported) ) -1
    private.schools$number
    
    t.test(public.schools$number, private.schools$number)
    
  # bestMCMC
    sample(public.schools$number, 100, replace = FALSE)  
    public.sample <- sample(public.schools$number, 100, replace = FALSE)
    public.sample
    
    sample(private.schools$number, 100, replace = FALSE)  
    pri.sample <- sample(private.schools$number, 100, replace = FALSE)
    pri.sample
    
    bayesian.test.problem.2 <- BESTmcmc(public.sample, pri.sample)
    bayesian.test.problem.2
    summary(bayesian.test.problem.2)
    plot(bayesian.test.problem.2)
    
    # data used for the BESTmcmc function was sampled at n = 100 due to the function taking a significant amount of time to
    # run.  
    # The observed p-value from the frequentist t-test provides a value of 2.2e-16, which suggests
      # statistical significance.  The confidence interval also ranges from 0.1091376 to 0.1448623.
      # Although this frequentist confidence interval is a 'long-term' solution (which may or may not apply
      # to this specific time the test was ran), we do not see that this interval spans across zero, which
      # also gives us confidence that we can reject the null hypothesis.  There does seem to be a credible
      # difference in overall reporting between public and private schools.  In order to run the BESTmcmc() command
      # the dataset was trimmed and sampled.  From the results of the bayesian test, we see a mean difference that
      # does unfortunately span across zero, with the 95% HDI showing ~-6 on the lower bound and ~6 on the upper bound.
      # I believe the trimmed dataset unfortunately may have skewed the results, so the frequentest perspective will
      # be viewed as more legitimate, with statistical significance over the smaller mean difference.  Regardless,
      # the difference does not seem to be very large.
  
  
  ############################################

# 3. What are 2013 vaccination rates for individual vaccines (i.e., DOT, Polio, MMR, and
# HepB) in California public schools? How do these rates for individual vaccines in
# California districts compare with overall US vaccination rates (make an informal
# comparison to the final observations in the time series)?
    
    str(districts)
    summary(districts)
    head(districts)
    plot(districts)
    
    str(allSchoolsReportStatus)
    View(allSchoolsReportStatus)
    
    allSchoolsReportStatus[allSchoolsReportStatus$name == 'Cajon Valley Union']
    
    districts[districts$DistrictName == 'Cajon Valley Union',]
    
    
    colnames(allSchoolsReportStatus)
    colnames(districts)
    districts.2013 <- districts
    districts.2013$name <- toupper(districts[,1])
    districts.2013$name
    
    districts.2013 <- merge(allSchoolsReportStatus, districts.2013, by = 'name')
    districts.2013
    
    districts.2013.public <- districts.2013[districts.2013$pubpriv == 'PUBLIC',]
    districts.2013.public
    
    plot(districts.2013.public$PctUpToDate)
    
    100 - mean(districts.2013.public$WithoutDTP)
    # 91.56842% for DTP in 2013 public schools
    100 - mean(districts.2013.public$WithoutPolio)
    # 91.85614for Polio in 2013 public schools
    100 - mean(districts.2013.public$WithoutMMR)
    # 91.37544% for MMR in 2013 public schools
    100 - mean(districts.2013.public$WithoutHepB)
    # 93.13684 for HepB in 2013 public schools
    
    tail(usVaccines)
    
    # A merge took place of the allSchoolsReportStatus and the districts dataset.  This merge took place in order
    # to identify vaccination rates for 2013 public school districts.  From here, since the rates are categorized as 
    # a percentage of those who did not receive the vaccines, the rates were ultimately subtracted from 100 to provide
    # a rate of those who did get vaccinated.
    # We see that the vaccination rate, on average, was 91.56842% for DTP in 2013 public schools, 91.37544% for MMR in 2013 public schools,
    # 91.37544% for MMR in 2013 public schools, and 93.13684 for HepB in 2013 public schools.
    # These rates are comparableto the ending of the dataset except for HepB_BD -- where the average of HepB
    # is significantly greater than the last remaining value in the usVaccines dataset for HepB.
    
  ############################################

# 4. Among districts, how are the vaccination rates for individual vaccines related? In other
# words, if students are missing one vaccine are they missing all of the others?

    str(usVaccines)
    plot(usVaccines)
    districts.vaxes <- districts[,2:5]
    cor(districts.vaxes)
    str(cor(districts.vaxes))
    
    cor.test(districts.vaxes$WithoutDTP, districts.vaxes$WithoutPolio)
    
    bfCorTest <- function (x,y) #Get r from BayesFactor
    {
      zx <- scale(x) #Standardize X  
      zy <- scale(y) #Standardize Y  
      zData <- data.frame(x=zx,rhoNot0=zy) #Put in a data frame  
      bfOut <- generalTestBF(x ~ rhoNot0, data=zData) #linear coefficient  
      mcmcOut <- posterior(bfOut,iterations=10000) #posterior samples  
      print(summary(mcmcOut[,"rhoNot0"])) #Show the HDI for r  
      return(bfOut) #Return Bayes factor object  
    }
    
    bfCorTest(districts.vaxes$WithoutDTP, districts.vaxes$WithoutPolio)
    
    # Among the districts, when looking at a correlation matrix, we see high degrees of correlation between the 
    # different attributes variables.  Since these variables are categorized as those who did not receive the vaccine,
    # this suggests that, if one student is to miss the DTP vaccine, for example, it is highly likely they will
    # also miss the Polio vaccine (correlation value of 0.9803893).  If we dive deeper into this example of DTP and Polio,
    # we find that both the frequentest approach as well as Bayesian point to similar results (that the variables are highly
    # correlated).  We see a p-value for the frequentest approach come out to be 2.2e-16, which suggests statistical
    # significance when comparing it to our conventional threshold of 0.05.  When performing the Bayesian test, we
    # see a rhoNot0 value of 4.937621e+489 to 1, which is extremely high.  If we remember our standard for determining
    # strong odds for this test, we would indicate anything greater than 150 to 1 would be strong odds.  To reiterate,
    # although we did just two vaccines in this example, we can see other correlation measures in the standard cor() command.
    
############################################

# 5. What variables predict whether or not a district's reporting was complete?
    
    districts.lm.data <- districts[,2:13]
    
    linear.model.5 <- lm(DistrictComplete ~ ., districts.lm.data)
    summary(linear.model.5)
    
    districts.lm.data$number <- as.numeric( factor(districts.lm.data$DistrictComplete) ) -1
    lmBF(number ~ ., districts.lm.data)
    
    # When comparing all variables to the 'DistrictComplete' attribute, we see that 2 variables have statistical
    # significance and may be sufficient predictors: (1) 'Enrolled' (p-value = 8.24e-11) and (2) 'TotalSchools (p-value = 2.20e-12).'  The intercept
    #  The intercept is also significant with a p-value at 0.006.  Although the overall p-value shown is 2.2e-16, we see that the R and
    # R-squared values are around 0.1, which doe snot suggest a strong model.
    
############################################

# 6. What variables predict the percentage of all enrolled students with completely up-to-
# date vaccines?
    
    
    colnames(districts.lm.data)
    
    linear.model.6 <- lm(PctUpToDate ~ ., districts.lm.data)
    summary(linear.model.6)
    
    bayesian.model.6 <- lmBF(PctUpToDate ~ WithoutDTP + WithoutMMR + PctBeliefExempt, data = districts.lm.data, posterior = TRUE, iterations = 10000)
    summary(bayesian.model.6)
    
    # When comparing all variables to the 'PctUpToDate' attribute, we see that 3 variables have statistical
    # significance and may be sufficient predictors: (1) 'WithoutDTP' (p-value = 6.41e-14) and (2) 'WithoutMMR (p-value = WithoutMMR),
    # (3) 'PctBeliefExempt' (p = 2.86e-11).
    #  The intercept is also significant with a p-value at 2e-16.  For the the R and
    # R-squared values, we see values of are around 0.94, which suggests a relatively strong model.  When running
    # the Bayesian version of this test on this predicting variables, we see similar results and do not see the 
    # HDI span across zero for any of the chosen attributes in the summary(bayesian.model.6) function.
    
############################################

# 7. What variables predict the percentage of all enrolled students with belief exceptions?
    
    
    colnames(districts.lm.data)
    
    linear.model.7 <- lm(PctBeliefExempt ~ ., districts.lm.data)
    summary(linear.model.7)
    
    bayesian.model.7 <- lmBF(PctBeliefExempt ~ WithoutDTP + WithoutHepB + PctUpToDate + PctFreeMeal, data = districts.lm.data, posterior = TRUE, iterations = 10000)
    summary(bayesian.model.7)
    
    # When comparing all variables to the 'PctBeliefExempt' attribute, we see that 4 variables have statistical
    # significance and may be sufficient predictors: (1) 'WithoutDTP' (p-value = 0.00116) and (2) 'WithoutHepB (p-value = 2e-16),
    # (3) 'PctUpToDate' (p = 2.86e-11), and (4) 'PctFreeMeal' (p = 0.00402).
    #  The intercept is also significant with a p-value at 1.91e-11.  For the the R and
    # R-squared values, we see values of are around 0.84/0.85, which suggests a relatively strong model.  When running
    # the Bayesian version of this test on this predicting variables, we see similar results and most of the variables do not 
    # have an HDI span across zero in the summary(bayesian.model.7) function.  It is important to note that we do see 
    # the 'PctFreeMeal' variable have an HDI of -0.01943 on the lower bound and 0.001079 on the upper bound.
    
    
############################################

# 8. What's the big picture, based on all of the foregoing analyses? The staff member in the
# state legislator's office is interested to know how to allocate financial assistance to
# school districts to improve both their vaccination rates and their reporting
# compliance. What have you learned from the data and analyses that might inform this
# question? 
      
      
      
      
