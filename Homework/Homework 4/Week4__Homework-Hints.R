cat('\014' # clear the console
rm(list=ls0) # Clear user objects from the environment

# Load the Plant Growth dataset
data (PlantGrowth)
? PlantGrowth

# Print a summary of the Plant Growth data
summary (PlantGrowth)

# Break the weights down by group
ctr1.weights = PlantGrowth$weight[PlantGrowth$group == 'ctrl']
trt1.weights = PlantGrowth$weight[PlantGrowth$group == 'trt1']
trt2.weights = PlantGrowth$weight[PlantGrowth$group == 'trt2']


# Plot a histogram of the control group weights
hist(ctr1.weights, main="Histogram of Control Group Plant Weights"
     ylab = "frequency"
     xlab=" weight (gms)"
     col="darkgreen"
     border = "white")

# Plot a histogram of the trtl diet weights
hist(trt1.weights, main="Histogram of TRT1 Group Plant Weights"
     lab = "frequency"


     # Create a boxplot of weights by group
     boxplot (weight ~ group, PlantGrowth
              main="Boxplot Chart of Plant Weights by Group"
              xlab=" group"
              ylab="weight (g)"
              col = : "palevioletred"
              border = "violetred4")
     
     # Generate a t test on the means between the control group
     t.test (ctrl.weights, trt1.weights)
     
     # Generate a t test on the means between the control group
     t.test (ctr1.weights, trt2.weights)








