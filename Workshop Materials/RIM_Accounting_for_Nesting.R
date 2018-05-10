#-------------------------------------------------------------------------------------
#
# FULL RANDOM INTERCEPT MODEL - ACCOUNTING FOR NESTING
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

#Set and double check our working directory
setwd("/Users/dylanwiwad/Desktop/MLM Seminar April 1-2, 2016/MLM Talk/Data")
getwd()

#Load up our data
ineq.data <- read.delim("Ineq.Tol data.dat", header = TRUE)

#-------------------------------------------------------------------------------------
#
# RUNNING OUR MODELS WITH AND WITHOUT ACCOUNTING FOR NESTING
#
#-------------------------------------------------------------------------------------

#Lets first run a regular regression, with sex, age, social standing, and education
#predicting tolerance for inequality.

reg.model <- lm(tol.ineq ~ SEX + AGE + social.class + EDUCYRS, data=ineq.data)
summary(reg.model)

#Take note of the b's and the extremity of the t-statistics

#Now, Lets run the model clustered by country
#Notice what we're doing here is adding in the clustering factor to a regular
#OLS regression - there are actually no level 2 predictors in the model
#Remember the ICC calculation told us there is
#significant clustering by country on tolerance for inequality

nested.model <- lme(tol.ineq ~ SEX + AGE + social.class + EDUCYRS, data=ineq.data, random = ~ 1|Country, method = "ML", na.action = "na.omit")
summary(nested.model)

#-------------------------------------------------------------------------------------
# Notice here that the b's are smaller and the t-statistics are substantially
# less extreme when we account for, but don't model the clustering in our DV
#-------------------------------------------------------------------------------------