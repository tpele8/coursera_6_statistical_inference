###### Clearing workspace ######
rm(list=ls())
cat("\014")
################################

library(datasets)
library(lattice)

### Comparing the Exponential distribution to the Uniform distribution ###
# Set random seed and constants
set.seed(1234)
lambda <- 0.2
rMean <- 1/lambda
rSD <- 1/lambda
n <- 40
numSims <- 1000

# Show convergence to the normal distribution over 40 simulations
  overallMean <- c()
  overallSD <- c()
  overallVar <- c()
  for(i in 1:n){
    expData <- rexp(n = i*numSims, rate = lambda)
    expDataMatrix <- matrix(expData, nrow = numSims, ncol = i)
    expDataMeanArray <- apply(expDataMatrix, 1, mean)
    overallMean <- c(overallMean, mean(expDataMeanArray))
    overallSD <- c(overallSD, sd(overallMean))
    overallVar <- c(overallVar, var(overallMean))
  }

  plot(overallMean)
  plot(overallSD)
  plot(overallVar)

  overallMean[length(overallMean)]
  overallSD[length(overallSD)]
  overallVar[length(overallVar)]

# Show distribution of means over final simulation:
  finalExpData <- rexp(n = n*numSims, rate = lambda)
  finalExpDataMatrix <- matrix(finalExpData, nrow = numSims, ncol = n)
  finalExpMeanArray <- apply(finalExpDataMatrix, 1, mean)
  finalExpVarArray <- apply(finalExpDataMatrix, 1, var)
  finalExpMean <- mean(finalExpMeanArray)
  finalExpSD <- sd(finalExpMeanArray)
  finalExpVar <- var(finalExpMeanArray)
  standError <- finalExpSD/sqrt(n)
  upperCI <- finalExpMean + 1.96*standError
  lowerCI <- finalExpMean - 1.96*standError
  finalExpMean
  finalExpSD
  finalExpVar
  

# Normal distribution
  normDist <- rnorm(n = numSims, mean = 1/lambda, sd = 1/lambda)
  normDistMean <- mean(normDist)
  normDistSD <- sd(normDist)
  normDistVar <- var(normDist)
  normDistMean
  normDistSD
  normDistVar

  hist(finalExpVarArray, xlab = "", main = "Hist of Exponential Distribution Variances")
  abline(v = normDistVar, col = "red", lwd = 4)
  legend(x = c(60, 77), y = c(290, 370), c("Exp", "Norm"), lty = c(1,1), col = c("black", "red"), lwd = c(2.5,2.5))
  mean(finalExpVarArray)

  par(mfrow = c(1,2))
  hist(finalExpMeanArray, xlab = "", main = "Hist of Exponential Distribution")
  abline(v = lowerCI, col = "red")
  abline(v = upperCI, col = "red")
  abline(v = normDistMean, col = "blue")
  hist(normDist, xlab = "", main = "Hist of Normal Distribution")

### Examining the ToothGrowth Data ###
# Loading data
  dev.off()
  df <- ToothGrowth
  
# Exploring the data
  head(df)  
  str(df)
  summary(df)

  plot(df$len, df$supp, col = df$dose, xlab = "Tooth Length", ylab = "Supplement Type")
  plot(df$len, df$dose, col = df$supp, xlab = "Tooth Length", ylab = "Dose size")
  hist(df$len)
  
  aggregate(len ~ dose, df, function(x) length(unique(x)))
  aggregate(len ~ supp, df, function(x) length(unique(x)))
  
# Visualizing the data
  boxplot(len~supp, data = df, xlab = "Supplement", ylab = "Tooth Length")
  boxplot(len~dose, data = df, xlab = "Dose", ylab = "Tooth Length")
  xyplot(len~supp|factor(dose), data = df, xlab = "Supplementation Method", 
         ylab = "Tooth Length", main = "Vitamin C Supplementation by Dose Size",
         col = c("red", "blue"))
  
  bwplot(len~supp|factor(dose), data = df, xlab = "Supplementation Method",
         ylab = "Tooth Length", main = "Vitamin C Supplementation by Dose Size",
         col = c("red", "blue"))
  
# Performing hypothesis testing
  # First test is checking to see if the dose size affects the tooth length
  # H0 = Tooth length is not affected by dose size
  # Ha = Tooth length is affected by dose size

  # Separate dose size into distinct subsets:
  doseSub1 <- subset(df, df$dose %in% c(0.5, 1.0))  
  doseSub2 <- subset(df, df$dose %in% c(0.5, 2.0))
  doseSub3 <- subset(df, df$dose %in% c(1.0, 2.0))
  
  # Perform single tailed T Tests on the subsetted data
  doseTTest1 <- t.test(len ~ dose, data = doseSub1)
  doseTTest2 <- t.test(len ~ dose, data = doseSub2)
  doseTTest3 <- t.test(len ~ dose, data = doseSub3)

  doseTTest1
  doseTTest2
  doseTTest3
  
  # Second test is checking to see if the supplement type affects tooth length
  # H0 = Tooth length is not affected by supplement type
  # Ha = Tooth length is affected by supplement type
  
  # Since there only exists 2 types of supplementation, we will not subset the dataset
  suppTTest <- t.test(len~supp, data = df)
  suppTTest
  
# Conclusion:
  # Since the p-value in the len/dose t tests were <<< 0.5, we can reject the null hypothesis
  # that the dose of vitamin C does not affect tooth length.  However, since the p-value in
  # the supplement test is > 0.5, we fail to reject the null hypothesis that the tooth length
  # is not affected by the supplement type.  We conclude that vitamin C is a valuable
  # supplement to help rodent's teeth grow longer, it does not matter if the supplement is
  # delivered through orange juice or ascorbic acid.