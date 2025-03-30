# Matthew L. Pergolski
# IST 772
# Homework 4
# 10/27/2021
# Dr. Block

############################################################

#The homework for Week 4 is exercises 7-10 on page 66.

#7. The built-in PlantGrowth data set contains three different groups, each representing
    #a different plant food diet (you may need to type data(PlantGrowth) to activate it). The
    #group labeled "ctrl" is the control group, while the other two groups are each a dif-
    #  ferent type of experimental treatment. Run the summary() command on PlantGrowth
    #and explain the output. Create a histogram of the ctrl group. As a hint about R syntax,
    #here is one way that you can access the ctrl group data:
      
    #  PlantGrowth$weight[PlantGrowth$group=="ctrl"]

    #Also create histograms of the trt1 and trt2 groups. What can you say about the differ-
    #ences in the groups by looking at the histograms?
          
          
          #show dataset
          data(PlantGrowth)
          PlantGrowth

          #summary command
          # the summary command shows that the minimum weight value is 3.590 while the max is 6.310; our mean is 5.073 and median is 5.155; this shows us the distribution of weight is slightly positively skewed between all control groups (since the means are not the same -- the mean is more sensitive to outliers)
          summary(PlantGrowth)
          
          #homework hint listed in question stored in variable
          control.group <- PlantGrowth$weight[PlantGrowth$group=="ctrl"]
          control.group
          
          #histogram of control group
          hist(control.group)
          
          #histogram of trt1
          trt1.group <- PlantGrowth$weight[PlantGrowth$group=="trt1"]
          trt1.group
          hist(trt1.group)
          
          #histogram of trt2
          trt2.group <- PlantGrowth$weight[PlantGrowth$group=="trt2"]
          trt2.group
          hist(trt2.group)
          
          # in looking at all of the histograms for the different control groups, I'd say that all have 'tails' extending towards the right-side of the diagrams, which suggests a positive skew; none of these would be considered a uniform distribution, as there is a central tendency in each of the control groups

  ############################################################


  #8. Create a boxplot of the plant growth data, using the model "weight ~ group." What
    #can you say about the differences in the groups by looking at the boxplots for the dif-
    #  ferent groups?
          
          boxplot(weight ~ group, data = PlantGrowth)
          
          # none of the boxplots are uniform (i.e., none of them look the same and are positioned similarly in the diagram).  Because of this, we can see that none of the Medians are the same, and the lower bounds are considerably off as well.  The upper bounds are closer, but still visually not equal.  All in all, the central tendency and dispersion of these independent samples are quite different from each other.  Trt2 distribution is higher in every dimension, while the trt1 distribution has the lowest dimensions out of the three; lastly, ctrl is about in the middle of the other two, in terms of distribution metrics.
  
  ############################################################

  #9. Run a t-test to compare the means of ctrl and trt1 in the PlantGrowth data. Report and
    #interpret the confidence interval. Make sure to include a carefully worded statement
    #about what the confidence interval implies with respect to the population mean differ-
     # ence between the ctrl and trt1 groups.
          
          mean(control.group)
          mean(trt1.group)
          t.test(control.group, trt1.group)
          
          # A 95% confidence interval of -0.2875162 and 1.0295162 was observed; we can conclude that, if this test was replicated 100 times, 95 of these replications would contain the true population mean and be represented within this interval of -0.2875162 and 1.0295162 -- or, at least, that's what we can estimate.  The point estimate, which is the difference of the two means listed in the t.test (i.e., 5.032 - 4.661 = 0.371), would be the value of 'choice,' per say. 
          # our band of uncertainty lies between the upper limit and the lower band.  The wider the interval, the more uncertain we are in this specific test.  We can not say for certain the true population value would be within this range (in the long run), since inferential statistics can not necessarily 'prove' anything for sure -- but we can use this information to be as confident as possible about determining the true population mean (or getting close to it, at least) from the data we have.

############################################################

#10. Run a t-test to compare the means of ctrl and trt2 in the PlantGrowth data. Report and
    # interpret the confidence interval.


          mean(control.group)
          mean(trt2.group)
          t.test(control.group, trt2.group)
          
          # A 95% confidence interval of -0.98287213 and -0.00512787 was observed; we can conclude that, if this test was replicated 100 times, 95 of these replications would contain the true population mean and be represented within this interval of -0.98287213 and -0.00512787 -- or, at least, that's what we can estimate.  The point estimate, which is the center point of the two means listed in the t.test (i.e., 5.032 - 5.526 = -0.4939), would be the value of 'choice,' per say. 
          # our band of uncertainty lies between the upper limit and the lower band.  The wider the interval, the more uncertain we are in this specific test.  We can not say for certain the true population value would be within this range (in the long run), since inferential statistics can not necessarily 'prove' anything for sure, but we can use this information to be as confident as possible about determining the true population mean (or getting close to it, at least) from the data we have.
          









