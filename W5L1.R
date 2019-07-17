
#In the age of big data, No of predictors > No of observations  or  No of observations not much larger than predictors
#Increased degrees of freedom, may have multiple solutions and estimators
#eg. f(x) = 5 + x1 + 2x2
#3 solutions to optimize prob
#Therefore too many predictors may not be a good thing. Need to drop those less useful

#Many ways to explain this, one way is bias-variance tradeoff
#Bias Variance Tradeoff
#Objective: to have a small Eout, good approx of out of sample (test) data.   Eout = irreducible error + variance of estimator + square of bias estimator
#Approx vs Generalization
#More Complex model: better chance of approx out of sample.        high variance, low bias (less assumptions abt form of target fn)
#Less Complex: better chance of generealizing out of sample.       low variance,  high bias (more assumptions abt form of target fn)
#*** trade-off, but try to minimize sum of variance and bias
#generalization test: gray region is variance

#Ways to find key predictors for Eout: subset selection  (since more complex uses more predictors, but performs poorer)
#Best subset selection (brute-force, check everything)  vs  Forward Stepwise selection (add 1 by 1, but may not get optimal) vs  Backward Stepwise selection (drop 1 by 1, the most useless predictor)
#Forward Stepwise selection: (increase vars one at a time)
#Fit for subsets where k =1, call best M1 
#Fit for subsets where k =2, call best M2
#Fit for subsets where k =3, call best M3
#Backward Stepwise selection: (drop vars one at a time)
#Fit for subsets where k =2, call best M2
#Fit for subsets where k =1, call best M1
#Fit for subsets where k =0, call best M0

#Best Subset Selection - Hitters.csv
rm(list=ls())         #remove all objs from current workspace
setwd("C:/Users/Hong Wen/documents")
hitters <- read.csv("Hitters.csv")
View(hitters)        #322 obs, 21 vars
str(hitters)         #League, Division and NewLeague are factors with 2 levels. Other vars are int, except for X
hitters <- na.omit(hitters)       #remove rows with NA
View(hitters)
str(hitters)         #263 obs, 21 vars

install.packages("leaps")        #leaps helps to do regression subset selection
library(leaps)
hitters <- hitters[,2:21]     #drops first column, which contains hitter names
model1 <- regsubsets(Salary~., data=hitters)  #default nvmax = 8
summary(model1)
model2 <- regsubsets(Salary~., data=hitters, nvmax = 19)    #check all subsets
summary(model2)
names(summary(model2))       #all the attibutes summary has: rsq, rss, adjr2
summary(model2)$rsq         #R^2 values for subsets of sizes 1,2,...19
plot(summary(model2)$rsq)
plot(summary(model2)$rss)      #Residual sum of squares
plot(summary(model2)$adjr2)
which.max(summary(model2)$adjr2)   #maximizer is the 11th subset
coef(model2,11)         #coeffs of the 11th subset (having 11 predictors)

#Forward Stepwise Selection (More efficient)
model3 <- regsubsets(Salary~., data=hitters, nvmax = 19, method = "forward")
summary(model3)
which.max(summary(model3)$adjr2)      #also 11th subset, same optimal solution as Best Subset Selection
plot(summary(model3)$adjr2)
summary(model2)$adjr2 - summary(model3)$adjr2      #difference between adjusted R^2 between model 2 and 3 for each subset of size 1,2,..19
max(summary(model2)$adjr2 - summary(model3)$adjr2)   #max difference in adjusted R^2 = 0.0009185854
#both models v similar results. but not always the case
coef(model3, 11)                           #coeff of subset containing 11 predictor vars 
max(coef(model2, 11)-coef(model3, 11))     #max diff in coefficients v small, therefore models largely the same

#Backward Stepwise Selection
model4 <- regsubsets(Salary~., data=hitters, nvmax = 19, method = "backward")
summary(model4)
which.max(summary(model4)$adjr2)     #11
plot(summary(model4)$adjr2)
summary(model2)$adjr2 - summary(model4)$adjr2  
max(summary(model2)$adjr2 - summary(model4)$adjr2)   #max difference = 0.010525
coef(model4, 11)
max(coef(model2, 11)-coef(model4, 11))     #max diff = 7.105427e^-13
