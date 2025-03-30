# IST 772
# HW 10
# MATTHEW PERGOLSKI
# DR. BLOCK
# 12/14/2021

############################################

# The homework for week 10 is exercises 2, 5, 6, 7, and 8 on pages 272 and 273.

############################################

# 2. Download and library the nlme package and use data ("Blackmore") to activate the
# Blackmore data set. Inspect the data and create a box plot showing the exercise level at
# different ages. Run a repeated measures ANOVA to compare exercise levels at ages 8, 10,
# and 12 using aov(). You can use a command like, myData <-Blackmore [Blackmore$age
#                                                                     <=12,1, to subset the data. Keeping in mind that the data will need to be bal-
#                                                                       anced before you can conduct this analysis, try running a command like this,
#                                                                     table(myData$subject,myData$age)), as the starting point for cleaning up the data set.
        
    library(car)
    library(nlme)
    
    myData <- Blackmore[Blackmore$age <= 12,]
    table(myData$subject,myData$age)
    myData
    
    boxplot(exercise~age, data = myData)
    
    data <- (myData[myData$age <= 12,])
    str(data)
    data$ageFact <- as.factor(data$age)
    list <- rowSums(table(data$subject,data$ageFact))==3
    list <- list[list == TRUE]
    list <- as.numeric(names(list))
    
    summary(data[data$ageFact == 8,])
    summary(data[data$ageFact == 10,])
    summary(data[data$ageFact == 12,])
    
    data <- data[data$subject %in% list,]
    summary(aov(exercise~ageFact+ Error(subject), data = data))
    
    # Interpretation
         #  The p-value of is statistically significant showing a value under the threshold of 0.05 (i.e., 1.33e-11)
          # This signifies that there is a significant difference in terms of the age attribute and exercise performed, which the median value is around 1.
            
############################################

# 5. Given that the AirPassengers data set has a substantial growth trend, use diff() to cre-
#   ate a differenced data set. Use plot() to examine and interpret the results of differenc-
#   ing. Use cpt.var() to find the change point in the variability of the differenced time series.
# Plot the result and describe in your own words what the change point signifies.
    
    install.packages('changepoint')
    library(changepoint)
    data('AirPassengers')
    air.pass <- diff(AirPassengers)
    plot(air.pass)
    
    cpt.var(air.pass)
    
    plot(cpt.var(air.pass))
    
    # Interpretation
      # The graph indicates a timeline in which data has been recorded in terms of passengers over several months/years
      # the red-line indicates an 'inflection point' in which the number of passengers began to substantially increase
      # versus before.  In looking at the graph from a visual perspective, we can see that the change happened
      # at some point between 1954 and 1956, presumably in 1955.  When looking at the function, we can see the 'Change
      # Point Locations' value equals 76.
    
############################################

# 6. Use cpt.mean() on the AirPassengers time series. Plot and interpret the results. Com-
#   pare the change point of the mean that you uncovered in this case to the change point
# in the variance that you uncovered in Exercise 5. What do these change points suggest
# about the history of air travel?
    
    air.pass.mean <- cpt.mean(air.pass, class = FALSE)
    plot(air.pass.mean)
    
    air.pass.mean['conf.value']
    
    # Interpretation
      # From the graph generated through plot(air.pass.mean), we see that there's a change from the first 
      # data point when compared to the data point that follows, we detect a clear difference in value, which
      # suggests a change has indeed been witnessed by those paying attention as time moved on.
    
  ############################################
  
#     7. Find historical information about air travel on the Internet and/or in reference materials
# that sheds light on the results from Exercises 5 and 6. Write a mini-article (less than 250
#                                                                               words) that interprets your statistical findings from Exercises 5 and 6 in the context of
# the historical information you found.
    
    # Interpretation
      # According to an article found on https://airandspace.si.edu/exhibitions/america-by-air/online/heyday/heyday11.cfm,
      # it indicates, "By the end of the 1950s...[flying] was becoming a necessity."
      # The article mentions that air traffic began to increase in the 1950s, which seems to match our historical
      # data found in R with the Airpassengers dataset.  When looking at the graph generated through R for 
      # the plot(air.pass.mean), we can confirm that the article's information suggests that our initial takeaways from our inflection/change point
      # are fairly accurate -- if not more tailored to the exact year when air travel become more 'popular.'
    
    
############################################

# 8. Use bcp() on the AirPassengers time series. Plot and interpret the results. Make sure to
# contrast these results with those from Exercise 6.
        
    install.packages('bcp')
    library(bcp)
    
    b.c.p <- bcp(as.vector(air.pass))
    b.c.p
    plot(b.c.p)
    
    # Interpretation
      # The findings from the Bayesian approach seem to draw similar conclusions.  We can see, from the posterior
      # probability distribution, we see that, from visually looking at the graph, that our change point seems to
      # correspond to the 60 through 80 interval period.
    
    
    
