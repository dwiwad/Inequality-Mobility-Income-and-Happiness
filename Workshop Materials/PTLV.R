#-------------------------------------------------------------------------------------
#
# PROPORTION OF THIRD LEVEL VARIANCE (PTLV)
#
#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
#
# SET UP STUFF
#
#-------------------------------------------------------------------------------------

#Set and double check our working directory
setwd("/Users/dylanwiwad/Desktop/MLM Seminar April 1-2, 2016/MLM Talk/Data")
getwd()

#Load up the packages we need
library(ICC)
library(foreign)
library(nlme)

#Get data
#This is a slightly different function than I usually use to read data in. This is just
#the way to read in .dta files, that were originally meant for stata.
incomedata <- read.dta("3L_income.dta", convert.dates = TRUE, convert.factors = TRUE, missing.type = FALSE, convert.underscore = FALSE, warn.missing.labels = TRUE)

#-------------------------------------------------------------------------------------
#
# CALCULATING ICC
#
#-------------------------------------------------------------------------------------
#Creating the null model - nothing but our DV (income), and second level factor, state

null.model <- lme(income~1, data=incomedata, random = ~ 1|state, method = "ML", na.action = "na.omit")
null.model

#Turn our second level, state, into a factor for the ICC calculation

incomedata$state <- as.factor(incomedata$state)

#Calculate ICC

ICCest(state, income, data = incomedata, alpha = .05, CI.type = "THD")

#-------------------------------------------------------------------------------------
#Returns a huge ICC, .27, suggesting that there is indeed clustering of 
#construction workers income by state
#Now, lets complicate the model by adding in another third level grouping factor,
#region
#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
#
# CALCULATING PTLV
#
#-------------------------------------------------------------------------------------

#Turn our third level, region, into a factor for the lme function

incomedata$region <- as.factor(incomedata$region)

#PTLV model

three.level.model <- lme(income ~ 1, data=incomedata, random = ~ 1|region/state, method = "ML", na.action = "na.omit")
three.level.model

#Use the variance information from our 3 level nested model to calculate the PTLV, 
#using the formula from slide number 30

PTLV <- .4624092^2/(.8439511^2 + .2087944^2 + .4624092^2)
PTLV

#-------------------------------------------------------------------------------------
#These data suggest that we need a 3-level model. There is a fairly significant
#amount of clustering at the regional level, PTLV = .22
#-------------------------------------------------------------------------------------


