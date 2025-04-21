## HW 12 Code
## Caroline Devine

####################################################################
# Question 1
####################################################################
library(tidyverse)
# experiment over 30 months
# X1,..., X30 = observations for each month 
# mean is unknown
# one-sided test (greater)
# Want to check if the t statistic at observation 20 provides statistically 
# discernible support for Ha

## Part A ##
n <- 20
val.20 <- qt(0.95,
          df = n-1)
# To produce a one-sided p-value that is below 0.05 such that it provides 
# statistically discernible support for the alternative hypothesis, the value of t_20
# would be 1.729 or greater. Since the researchers set up a right-tailed test, to 
# reject the null, we need to find the t-statistic that is equal to 1.73 or larger, 
# meaning beyond 0.05. 


## Part B ##
n2 <- 30
val.30 <- qt(0.95,
             df = n2 -1)

# For statistically discernible support of the alternative hypothesis, 
# we would need the t-test statistic to be 1.699 or larger.

## Part C: Laplace distribution a = 0, b = 4.0 ##
library(VGAM)
a <- 0
b <- 4.0 
mu0 <- 0
n.simulations <- 1000
type1.count <- 0 

for (i in 1:n.simulations){
  simulations <- rlaplace(n = 30, location = a, scale = b)
  
  # Up to 20 months
  first.20 <- simulations[1:20]
  t <- t.test(first.20,
              mu = mu0,
              alternative = "greater")
  
  if (t$p.value < 0.05){
    type1.count <- type1.count + 1
  }else{
    
    # 30 months
    t.2 <- t.test(simulations,
                  mu = mu0,
                  alternative = "greater")
    if (t.2$p.value < 0.05){
      type1.count <- type1.count + 1
    }
  }
}

type1.error <- type1.count/n.simulations

# A type 1 error is when when we incorrectly find statistically discernible 
# support for the alternative hypothesis. This means that the null hypothesis 
# is true and we reject the null hypothesis in favor of the alternative. 
# The researchers assume the null hypothesis to be mu0 = 0. For a type 1 error to occur in this experiment, 
# we want to reject the null when it is actually true, meaning the true mean is 0 but our simulated mean does not reflect that
# To look at both observations (when there are 20 observations versus 30 at the end of the experiment)
# we simulated how many times the probability value is less than alpha which is 0.05 for the first 20 observations
# and then if there are no rejections of the null hypothesis, we tested for the full 30 observations.
# The rate of getting a type 1 error using the simulation approach is 0.074. 


####################################################################
# Question 2
####################################################################

# preform simulation study for assessment of robustness of T test
n <- 15
n.simulations <- 10000

## Part A: Proportion of Time we make a Type 1 error for left-tailed test ##

# Beta (10,2)
true.mean1 <- 10/(10+2)
# Beta (2,10)
true.mean2 <- 2/(2+10)
# Beta (10,10)
true.mean3 <- 10/(10+10)

# Counters
leftcount <- c(0,0,0)
rightcount <- c(0,0,0)
dualcount <- c(0,0,0)

for (i in 1:n.simulations){
  beta1 <- rbeta(n, 10, 2)
  beta2 <- rbeta(n, 2, 10)
  beta3 <- rbeta(n, 10, 10)
  
  # Left-Tailed Test
  left1 <- t.test(beta1,
                  mu = true.mean1,
                  alternative = "less")
  left2 <- t.test(beta2,
                  mu = true.mean2,
                  alternative = "less")
  left3 <- t.test(beta3,
                  mu = true.mean3,
                  alternative = "less")
  
  # Type 1 Error Count
  leftcount[1] <- leftcount[1] + (left1$p.value < 0.05)
  leftcount[2] <- leftcount[2] + (left2$p.value < 0.05)
  leftcount[3] <- leftcount[3] + (left3$p.value < 0.05)
  
  
  # Right-Tailed Test
  right1 <- t.test(beta1,
                  mu = true.mean1,
                  alternative = "greater")
  right2 <- t.test(beta2,
                  mu = true.mean2,
                  alternative = "greater")
  right3 <- t.test(beta3,
                  mu = true.mean3,
                  alternative = "greater")
  
  # Type 1 Error Count
  rightcount[1] <- rightcount[1] + (right1$p.value < 0.05)
  rightcount[2] <- rightcount[2] + (right2$p.value < 0.05)
  rightcount[3] <- rightcount[3] + (right3$p.value < 0.05)
  
  
  # Two-Tailed Test
  dual1 <- t.test(beta1,
                   mu = true.mean1,
                   alternative = "two.sided")
  dual2 <- t.test(beta2,
                   mu = true.mean2,
                   alternative = "two.sided")
  dual3 <- t.test(beta3,
                   mu = true.mean3,
                   alternative = "two.sided")
  
  # Type 1 Error Count
  dualcount[1] <- dualcount[1] + (dual1$p.value < 0.05)
  dualcount[2] <- dualcount[2] + (dual2$p.value < 0.05)
  dualcount[3] <- dualcount[3] + (dual3$p.value < 0.05)
  
}

