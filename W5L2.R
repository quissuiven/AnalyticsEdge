
#k-fold cross validation
#forbidden to use test data to test model
#what if create fake test sets: validation sets?
#trying all combinations setting data as training and validation, find avg error, pick best model

#Least Absolute Shrinkage and Selection Operator (Optimization approach for subset selection, improves accuracy of prediction)
#want to minimize function: general subset selection + sum of absolute values of coeffs
#how to penalize complexity in the model?
#How to quantify complexity of the model?
#1. Number of predictors in the model
#2. Sum of absolute values of coefficients, weighted with nonnegative lambda  (if sum is high, complex model)

#B1: Training
rm(list =ls())
install.packages("glmnet")
library(glmnet)
hitters <- read.csv("Hitters.csv")
hitters <- na.omit(hitters)
hitters <- hitters[,2:21]
x <- model.matrix(Salary~., hitters)     #convert LeagueN, DivisionW, NewLeagueN to 0s and 1s. Since glmnet() works only w quant vars, in matrix format
x[1:263, 1:20]               #19 predictors, 1 intercept

y <- hitters$Salary
y[1:263]

seq(10, -2, length = 10)
grid <- 10^seq(10, -2, length = 10)   #use 10^10 to 10^-2 as range of candidate values for lambda in LASSO model.

set.seed(1)  #randomly partition data into training and test sets. Set rand no generator seed to 1 to get same results

train <- sample(1:nrow(x), nrow(x)/2)       #from x. take random sample of 131 indices
test <- -train               #remaining 131 indices becomes test data

modellasso <- glmnet(x[train,], y[train], lambda = grid)   #x[train] is input matrix, y[train] is response var, lambda is lambda sequence
#solves model for 10 values of lambda provided in grid

plot(modellasso, xvar = "lambda")   #plots coefficient values for diff values of lambda
#as lambda increases, value of coefficients tend to zero

modellasso$df   #df is number of nonzero coefficients for ea value of lambda

modellasso$beta  #finding beta values for each lambda value

coef(modellasso) #finding intercept term

#B2: Initial Tests
predictlasso1 <- predict(modellasso, newx = x[test,], s= 100)        #s is lambda, ask for result where lambda = 100. Lambda = 100 not in grid, so use linear interpolation

mean((predictlasso1 - y[test])^2)   #find mean squared error: 126478.7 

predictlasso2 <- predict(modellasso, newx = x[test,], s = 200)      #test model for lambda = 200

mean((predictlasso2 - y[test])^2)   #find mean squared error: 128645.1

predict.glmnet(modellasso, newx = x[test,])              #see predictions for all lambda in the grid

predictlasso3 <- predict(modellasso, s= 0, newx = x[test,], exact = T, x=x[train,], y= y[train])        #retrain since lambda = 0 not in previous grid, so supply original data to refit model

mean((predictlasso3 - y[test])^2)     #find mean squared error: 115292.6

predictlasso4 <- predict(modellasso, s=10^10, newx = x[test,], exact = T)     #lambda 10^10 is included in the grid

mean((predictlasso4 - y[test])^2) 

#different lambdas affect quality of fit (mean squared error). Do cross validation to find best value of lambda


#B3: Cross validation

set.seed(2)

cvlasso <- cv.glmnet(x[train,], y[train])  #does 10-fold cross validation, returns lambda sequence

cvlasso$lambda.min   #35.32063, optimal value of lambda since wan to minimize entire function

predictlassocv <- predict(modellasso, s= 22.18238, newx = x[test,])    #predict using most optimal lambda

mean((predictlassocv - y[test])^2)     #96106.57, smallest mean squared error

lassocoef <- predict(modellasso, s = 22.1823, type = "coefficients")

#summary: 
#1. Create modelmatrix for lasso model
#2. Create grid of lambda sequences
#3. separate into training set and test set
#4. Create lasso model
#5. Use cross validation to find best lambda (with the smallest error)
#6. Predict using best lambda
#7. Find mean squared error and coefficients

#C: Cross-Country Growth Regressions - economicgrowth.csv

#C1: best subset selection
rm(list = ls())
eg <- read.csv("economicgrowth.csv")
str(eg)                #72 observations, 43 vars(country, y, 41 other vars)
eg1 <- subset(eg, select = -c(Country))    #drops column country
library(leaps)

model1 <- regsubsets(y~., data=eg1, nvmax = 41)      #exhaustive search over 2^41 subsets. Takes 5 mins, too complex model

plot(summary(model1)$rsq)
plot(summary(model1)$adjr2)
plot (model1, scale = c("r2"))          #non-monotonicity

#C2: Forward Stepwise Selection
model2 <- regsubsets(y~. data= eg1, nvmax = 41, method = "forward")

plot(summary(model2)$rsq)
plot(summary(model2)$adjr2)        #different from model1
plot (model2, scale = c("r2"))      #monotone, since vars are added and nv removed

summary(model1)$which        #shows which vars are included in each step

summary(model2)$which

#C3: LASSO

x <- model.matrix(y~., eg1)   #convert to matrix form
library(glmnet)
model3 <- glmnet(x,eg1$y)    #solves lasso model, use default lambda values
model3$lambda
model3$df
model3$beta != 0 
