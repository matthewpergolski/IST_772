# Matthew L. Pergolski
# IST 772
# Homework 3
# 10/25/2021
# Dr. Block

############################################################
  
  #The homework for week three is exercises 2 through 7 on pages 50 and 51.
  
  #2. For the remaining exercises in this set, we will use one of R’s built-in data sets, called  the “ChickWeight”
      #data set. According to the documentation for R, the ChickWeight  data set contains information on the 
      #weight of chicks in grams up to 21 days after hatch‑  ing. Use the summary(ChickWeight) command to reveal 
      #basic information about the  ChickWeight data set. You will find that ChickWeight contains four different 
      #variables.  Name the four variables. Use the dim(ChickWeight) command to show the dimensions  of the 
      #ChickWeight data set. The second number in the output, 4, is the number of col‑  umns in the data set, 
      #in other words the number of variables. What is the first number?  Report it and describe briefly what you 
      #think it signifies. 
      ############################################################
          
          #calling the dataset
          ChickWeight
          
          #summary command reveals variable names: (1) weight; (2) Time; (3) Chick; (4) Diet
          summary(ChickWeight)
          
          #dim function reveals the number of observations (578) as well as the variables or attributes (4);
          #more specifically, the observations (i.e., rows) equate to the number of chick weights that were recorded
          dim(ChickWeight)
          

  #3. When a data set contains more than one variable, R offers another subsetting operator,  $, 
      #to access each variable individually. For the exercises below, we are interested only  in the contents 
      #of one of the variables in the data set, called weight. We can access the  weight variable by itself, 
      #using the $, with this expression: ChickWeight$weight. Run  the following commands, say what the command 
      #does, report the output, and briefly  explain each piece of output:  
      #summary(ChickWeight$weight)  
      #head(ChickWeight$weight)  
      #mean(ChickWeight$weight)  
      #myChkWts <- ChickWeight$weight  
      #quantile(myChkWts,0.50) 
      ############################################################
          
          #summary function provides insight into the min, max, mean, median, and 1st/3rd quartiles of the dataset
          summary(ChickWeight$weight)  
          
          #head function reveals the values of the dataset towards the top for a given vector
          head(ChickWeight$weight)
          
          #mean function calculates the average of the values for a given vector
          mean(ChickWeight$weight)  
         
          #'myChckWts' is a variable where information is stored; in this instance, the weight vector of the ChickWeight data is stored in this variable
          myChkWts <- ChickWeight$weight 
          
          #quantile function provides a value corresponding to a specific percentage of the data specified from the user; in this instance, the value associated with the 50% mark of the data is returned
          quantile(myChkWts,0.50) 

          
          
          
  #4. In the second to last command of the previous exercise, you created a copy of the  weight data from the 
      #ChickWeight data set and put it in a new vector called myChkWts.  You can continue to use this myChkWts 
      #variable for the rest of the exercises below. Cre‑  ate a histogram for that variable. Then write code 
      #that will display the 2.5% and 97.5%  quantiles of the distribution for that variable. Write an 
      #interpretation of the variable,  including descriptions of the mean, median, shape of the distribution, 
      #and the 2.5%  and 97.5% quantiles. Make sure to clearly describe what the 2.5% and 97.5% quantiles  signify. 
      ############################################################

          #calling dataset
          myChkWts
          
          #histogram of dataset
          hist(myChkWts)
          
          #summary function for dataset
          summary(myChkWts)
          
          #quantiles of dataset
          quantile(myChkWts, c(0.025, 0.975))
          
          #interpretation of myChkWts
          #the variable myChkWts is a variable that contains the column data of chick weights from the ChickWeight dataset
          #a histogram function was put forth on the myChkWts variable and produced an output of a positively skewed distribution
          #we can tell it's positively skewed by the 'tail' that forms on the right-hand side of the illustration
          #the shape of the distribution can also be described with the summary function; this informs us that the mean (121.8) is higher than the median (103)
          #which makes sense since the mean is more 'vulnerable' to outliers vs the median
          #for the quantiles, we get a value of 41 for .025; this means that 2.5% of the data in this dataset is less than or equal to 41 units
          #similarly, we get a value of 294.575 for the value corresponding to the quantile of .975, meaning that 97.5% of the data is less than or equal to 294.575 units.
          
          
          
          
  #5. Write R code that will construct a sampling distribution of means from the weight data (as  noted above, 
      #if you did exercise 3 you can use myChkWts instead of ChickWeight$weight  to save yourself some typing). 
      #Make sure that the sampling distribution contains at least  1,000 means. Store the sampling distribution 
      #in a new variable that you can keep using.  Use a sample size of n = 11 (sampling with replacement). 
      #Show a histogram of this distribution of sample means. Then, write and run R commands that will display 
      #the  2.5% and 97.5% quantiles of the sampling distribution on the histogram with a vertical  line. 
      ############################################################

          chick.var <- replicate(1000, mean(sample(myChkWts, size = 11, replace = TRUE)), simplify = TRUE)
          chick.var
          hist(chick.var)
          
          quantile(chick.var, c(.025, .975))
          abline(v = quantile(chick.var, c(.025, .975)))
          
          
  #6. If you did Exercise 4, you calculated some quantiles for a distribution of raw data. If you
    #did Exercise 5, you calculated some quantiles for a sampling distribution of means.
    #Briefly describe, from a conceptual perspective and in your own words, what the differ-
    #  ence is between a distribution of raw data and a distribution of sampling means. Finally,
    #comment on why the 2.5% and 97.5% quantiles are so different between the raw data
    #distribution and the sampling distribution of means.
    ############################################################
        
          #In this instance, we are comparing a histogram of raw data versus sampling data that was replicated many times over.  The raw data showed a skewed distribution while the replicated sampling distribution showed what appears to be more of a ‘normal’ curve.  My initial thoughts are that the raw data is not subject to the central limit theorem (and law of large numbers) like the replicated sampling process is.  As the sample size (assuming with replacement) increases – along with a larger amount of ‘trials’ – we can observe a dataset reflect something similar to a normal distribution.  Since the raw data is not replicated, and is considered one ‘trial,’ we would not necessarily expect to see normal curve when plotting the dataset in a histogram.
          summary(myChkWts)
          hist(myChkWts)
          
          summary(chick.var)
          hist(chick.var)
          
  #7. Redo Exercise 5, but this time use a sample size of n = 100 (instead of the original
      #sample size of n = 11 used in Exercise 5). Explain why the 2.5% and 97.5% quantiles
      #are different from the results you got for Exercise 5. As a hint, think about what makes
      #a sample "better."
      ############################################################

          chick.var.2 <- replicate(1000, mean(sample(myChkWts, size = 100, replace = TRUE)), simplify = TRUE)
          chick.var.2
          hist(chick.var.2)
          
          quantile(chick.var.2, c(.025, .975))
          abline(v = quantile(chick.var.2, c(.025, .975)))
          
          summary(chick.var)
          summary(chick.var.2)
          
          quantile(chick.var, c(.025, .975))
          quantile(chick.var.2, c(.025, .975))
          

          #In this instance, we are comparing two different distributions of replicated sample means.  One has a sample size of 11 (with 1,000 trials) while the other has a sample size of 100 (with 1,000 trials).  The output for quantiles 2.5 and 97.5% are different – and the reasoning for this would simply be the law of large numbers.  As the number of samples increase, we would expect to see a more ‘perfect’ normal distribution, essentially a more ‘refined’ or ‘accurate’ illustration showing the mean, median, mode, etc.



