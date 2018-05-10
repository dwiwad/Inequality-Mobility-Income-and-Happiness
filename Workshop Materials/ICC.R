#-------------------------------------------------------------------------------------
#
# TESTING FOR THE NEED FOR MLM - CALCULATING THE ICC
#
#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
#
# SET UP STUFF
#
#-------------------------------------------------------------------------------------

#Load any relevant packages

library(car)
library(ggplot2)
library(nlme)
library(reshape)
library(ICC)

#the setwd() function tells R where on your computer to grab things from
#the getwd() command just displays it for us so we can double check its 
#set at the right place

setwd("/Users/dylanwiwad/Desktop/MLM Seminar April 1-2, 2016/MLM Talk/Data")
getwd()

#Tell R to read a .dat file, from your working directory and load it 
#under the name "Ineq.data"

Ineq.data <- read.delim("ineq.tol data.dat", header = TRUE)

#-------------------------------------------------------------------------------------
#
# CALCULATING THE ICC
#
#-------------------------------------------------------------------------------------

#This tests the null model, in which there are no predictors entered. Only the DV is
#put in, the model remains "empty (null);" intercept only
#The key points to know in replicating this in the future are the general form:
#lme(y~1, data, specification of the nesting factor, what to do with missing data)
#Here, y is tolerance for inequality, data is Ineq.data, and we are specifying one random
#effect, which is the country, and we are telling it to omit missing data (listwise deletion)

null.model <- lme(tol.ineq~1, data=Ineq.data, random = ~ 1|Country, method = "ML", na.action = "na.omit")
null.model

#this is for computing the ICC. The ICC is the proportion of variance in y explained
#by the nesting factor relative to the total variance in y. That is, from the model
#summary from above, this is, under random effects: intercept^2/(intercept^2 + residual^2)
#MORE PROPERLY: rho = TAUnaughtnaught = (TAUnaughtnaught + sigma squared)
#The reason they are squared is because the intercept and residual values given to us
#are standard deviations, thus we need to turn them back into variances, then compute
#the ICC

ICC = .3006613^2/(.3006613^2 + .8979297^2)
ICC

#Alternatively, we can calculate the ICC using the function
#Calculate the ICC using the function contained in the ICC package
#We also get a confidence interval

ICCest(Country, tol.ineq, data = Ineq.data, alpha = .05, CI.type = c("THD"))

#-------------------------------------------------------------------------------------
# As we can see from the output, the ICC is sufficiently high, about .10, to justify
# the use of multilevel modeling - we must account for the clustering by country
#-------------------------------------------------------------------------------------