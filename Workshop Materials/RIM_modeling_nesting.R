#-------------------------------------------------------------------------------------
#
# FULL RANDOM INTERCEPT MODEL
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
# RUNNING OUR MODELS WITH AND WITHOUT ADDITIONAL COVARIATES
#
#-------------------------------------------------------------------------------------

#Rebuild the null model for reference; no covariates

null.model <- lme(tol.ineq~1, data=ineq.data, random = ~ 1|Country, method = "ML", na.action = "na.omit")
null.model

#Now, we can start tossing in covariates. Let's just test our initial model:
#Does country level income mobility predict individual tolerance for inequality?

RI.model <- lme(tol.ineq ~ inc.elast, data=ineq.data, random = ~ 1|Country, method = "ML", na.action = "na.omit")
summary(RI.model)

#Plotting all of the data of the RIM model, just to visualize, same slope
#different intercept for each country

RIM.graph <- ggplot(ineq.data, aes(tol.ineq, inc.elast)) + opts(title="Tolerance for inequality and income elasticity in 19 Countries")
RIM.graph + geom_point(aes(colour = Country)) + geom_smooth(aes(colour = Country), method = "lm", se = F) + labs(x = "Tolerance for Inequality", y = "Income Elasticity")

#-------------------------------------------------------------------------------------
#Okay - so it appears that as incomes become more mobile (elasiticity goes down)
#tolerance for inequality goes up, b=-1.22, SE=.38, t(17) = -3.27, p=.004  
#just like we predicted.
#-------------------------------------------------------------------------------------

#We want to model in some other level 2 covariates - what 
#characteristics of the country actually matter here in predicting tolerance
#for inequality. We want to factor in GDP and actual inequality in the country

RI.full.model <- lme(tol.ineq ~ inc.elast + GDP_Percap + Gini_Latest, data=ineq.data, random = ~ 1|Country, method = "ML", na.action = "na.omit")
summary(RI.full.model)

#-------------------------------------------------------------------------------------
#As can be seen from the output, the impact of income mobility on tolerance for
#inequality actually gets stronger, b=-2.05, SE=.56, when we control for GDP and
#income inequality
#-------------------------------------------------------------------------------------

