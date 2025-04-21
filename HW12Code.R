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

# 

