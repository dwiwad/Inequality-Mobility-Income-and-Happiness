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
# RUNNING A MODEL WITH LEVEL 1 AND LEVEL 2 PREDICTORS
#
#-------------------------------------------------------------------------------------

#All we're going to do now is run a combination of the same two models we ran before
#Exact same code. We're just going to include both level 1 and level 2 predictors

full.model <- lme(tol.ineq ~ inc.elast + GDP_Percap + Gini_Latest + SEX + 
                    AGE + social.class + EDUCYRS, data=ineq.data, 
                  random = ~ 1|Country, method = "ML", na.action = "na.omit")
summary(full.model)
