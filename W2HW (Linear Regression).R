
#Question 1
#Part a
#need to change horsepower since it is a factor, need to change to numeric to work with it
#factor is like a separate class, while horsepower itself is a number 
#thus when building linear model w factor, recognises as separate independent variable

auto = read.csv("Auto.csv")

auto$horsepower <- as.numeric(as.character(auto$horsepower))         #convert horsepower column from factor to numeric

model1= lm(mpg~ horsepower, auto)
summary(model1)


#Part b 
#if p value < alpha value (area), 
#reject null hypothesis (B = 0),                       => horsepower and mpg not related  (since y = Bx + c. if B = 0, not related)
#so statistically significant that B != 0              => horsepower and mpg related

#if p-value close to 0          => strong relationship
#if p-value close to alpha      => weak relationship

#since B = -0.157845, correlation is negative


#Part c
#confidence level + significance level/alpha = 1  eg. 0.95 + 0.05 = 1
#confidence interval is range of likely values for a population parameter such as population mean
#eg. 99% confidence level: horsepower ranges from -0.1745289 to -0.1411606

#if horsepower =98, mpg =?

newdata = data.frame(horsepower = 98)         #create new matrix containing 98, test set
predict(model1, newdata)                      #input x = 98 into equation, to get y (mpg)   => 24.46708

#ab = predict(model1,auto)      #this is wrong, since need to predict on test set, not training set. mpg = 23.362164

#what is 99% confidence interval for mpg?

confint(model1,level = 0.99)    #this only tells confidence level of predictor, not response
predict(model1, newdata, interval = c("confidence"), level = 0.99)       #=> fit is exact value, varies from 23.81669 to 25.11747


#Part d
cor(auto$mpg, auto$horsepower, use = "pairwise.complete.obs")    #compute correlation, while dropping obs where one entry missing => -0.7784
                                                                 # squaring correlation gives R^2

#Part e
plot(auto$horsepower, auto$mpg)
abline(model1)                            #abline draws line. abline(model1) draws linear regression line

#Part f
#residuals can show how poorly a model represents data
#4 types of diagnostic plots:
layout(matrix(1:4,2,2))
plot(model1)

#Residuals vs Fitted: Value decrease than increase
#if equally spread around horizontal line, good indication that good linear fit
#QQ plot: Not normal at extreme values, since not straight line, has some outliers


#Question 2
#part a
plot(auto)

#part b
cor(auto[1:8])                                   #exclude last column names

auto1 <- subset(auto, select = -c(name))         #alternatively
cor(auto1)


#part c
model2 = lm(mpg~ cylinders + displacement + horsepower + weight + acceleration + year + origin,auto)
summary(model2)
#Strong relationship since p-value is close to 0, therefore can reject null hypothesis that Bs are all zero
#displacement, weight, year and origin
#year is positively correlated with mpg

model2 = lm(mpg~.,auto1)                         #alternatively
summary(model2)




#Question 3
#part a
set.seed(1)
x1 <- runif(100)
x2 <- 0.5*x1 +rnorm(100)/10
y <- 2 + 2*x1 + 0.3*x2 + rnorm(100)

#the form of the linear model is y = 2 + 2x1 + 0.3x2 + error,  where B0 = 2, B1 = 2, B2 = 0.3

#part b
cor(x1,x2)         #0.8351
plot(x1,x2)        #strong positive correlation between x1 and x2

#part c
model3 <- lm(y~ x1 + x2)
summary(model3)                 #estimated B0 = 2.1305, estimated B1 = 1.4396, estimated B2 = 1.0097.      y = 2.1305 + 1.439x1 + x2
                                     #true B0 = 2, true B1 = 2, true B2 = 0.3     => bigger discrepancy for estimates of B1, B2 than B0 
                                            
#Reject null hypothesis B1 = 0 at 5% significance level since p < 5%
#Do not reject null hypothesis B2 = 0 at 5% significance level since p > 5%
  

#part d
model4 <- lm(y~ x1)            #estimated B1 = 1.9759, discrepancy between true B1 lesser.   y = 2.1124 + 1.9759 x1
summary(model4)
#Reject null hypothesis B1 = 0 since p < 5%

#part e
model5 <- lm(y~ x2)            #estimated B2 = 2.8996, discrepancy between true B2 bigger
summary(model5)
#Reject null hypothesis B2 = 0 since p < 5%

#part f
#The two independent variables may be correlated, therefore accuracy of the multicollinear model is weaker

#ans: there is multicollinearity in the data between x1 and x2. In multiple regression, hard to reject B1 = 0, whereas in single regression, can reject



#Question 4
#part a
boston = read.csv("boston.csv")

#no shortcut, need to create models 1 to 13
model4a <- lm(medv~ crim, boston)            
summary(model4a)

model4b <- lm(medv~ zn, boston)
summary(model4b)

model4c <- lm(medv~ indus, boston)
summary(model4c)

model4d <- lm(medv~ chas, boston)
summary(model4d)

model4e <- lm(medv~ nox, boston)
summary(model4e)

model4f <- lm(medv~ rm, boston)
summary(model4f)

model4g <- lm(medv~ age, boston)
summary(model4g)

model4h <- lm(medv~ dis, boston)
summary(model4h)

model4i <- lm(medv~ rad, boston)
summary(model4i)

model4j <- lm(medv~ tax, boston)
summary(model4j)

model4k <- lm(medv~ ptratio, boston)     
summary(model4k)

model4l <- lm(medv~ black, boston)      
summary(model4l)

model4m <- lm(medv~ lstat, boston)       
summary(model4m)

#all vars 3 stars, statistically significant
plot(boston$lstat,boston$medv)                  #as lstat increases, medv decreases
abline(model4m)


#part b
modela <- lm(medv~., boston)       #~. for multiple regression, similar to zn + indus + chas...
summary(modela)

#adjusted R^2 = 0.7338 higher than adjusted R^2 of individual simple regression models
#for crim, zn,chas, nox, rm, dis, rad, tax, ptratio, black, lstat can reject B = 0 at 5% sig level
  

#part c
model4a$coefficients[2]               #returns B1 
#store all B1s into a vector x 
x <- c(model4a$coefficients[2], model4b$coefficients[2], model4c$coefficients[2], model4d$coefficients[2], model4e$coefficients[2], model4f$coefficients[2],
       model4g$coefficients[2], model4h$coefficients[2], model4i$coefficients[2], model4j$coefficients[2], model4k$coefficients[2], model4l$coefficients[2],
       model4m$coefficients[2])

modela$coefficients                   #returns B0 to B13
y <- c(modela$coefficients[2:14])
plot(x,y, main="coefficient relationship", xlab = "simple linear regression", ylab = "multiple linear regression")
#fairly positive relationship, linear


#part d
poly(boston$lstat, degree=2, raw=TRUE)           #use raw numbers instead of orthogonal polynomials
model4d1 <- lm(medv~ poly(lstat, 2), boston)     #represents lstat + lstat^2
summary(model4d1)

model4d1 <- lm(medv~ lstat + poly(lstat, degree=2) + poly(lstat, degree=3) , boston)     #similar to below        
summary(model4d1)

model4d2 <- lm(medv~ poly(lstat, 3), boston)     #represents lstat + lstat^2 + lstat^3
summary(model4d2)

model4d7 <- lm(medv~ poly(lstat, 6), boston)     #6th var not significant, therefore beyond degree 5 adding additional terms not significant
summary(model4d7)


#Question 5
#part a
climate = read.csv("climate_change.csv")
climatetrain <- subset(climate, climate$Year <= 2006)
View(climatetrain)
climatetest <- subset(climate, climate$Year > 2006)

model1 <- lm(Temp~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, climatetrain)
summary(model1)                  #R^2 = 0.7509

#part b
#significant vars: MEI, CO2, CFC.11, CFC.12, TSI, Aerosols

#part c
cor(climatetrain)

#N2O quite correlated with year, CO2, CH4, CFC.11, Temp
#CFC.11 quite correlated with Year, CO2, CH4, N2O
#results indicate that all variables correlated in dataset


#part d
model2 <- lm(Temp~ MEI + TSI + Aerosols + N2O, climatetrain)
summary(model2)     #coeff of N2O is 2.532e-02 compared to -1.653e-02 in previous model
#model R^2 is 0.7261
#model does not lose a lot of explanatory power when no of vars reduced, since many independent vars correlated


#part e
#use step() to balance model simplicity, and model R^2
simplemodel <- step(model1)
summary(simplemodel)           #R^2 = 0.7508, vars eliminated is CH4


#part f
temppredictions <- predict(simplemodel,climatetest)
summary(temppredictions)

ssemodel <- sum((climatetest$Temp-temppredictions)^2)          #(true - predicted value)^2, and sum all up
sstmodel <- sum((climatetest$Temp-mean(climatetrain$Temp))^2)     #use Ytrain. More mathematically valid
1 - (ssemodel/sstmodel)    

#Test R^2 is 0.6286051


#Question 6
#part a
wine <- read.csv("winedata.csv")
str(wine)
wine$age91 <- 1991 - wine$vintage             #adding new var
wine$age92 <- 1992 - wine$vintage
winesubset = subset(wine, wine$age91 >= 15)
mean(winesubset$price91)               #96.4353

#part b
winesubset2 = subset(wine, wine$hrain < mean(wine$hrain)& wine$tempdiff < mean(wine$tempdiff))
mean(winesubset2$price91)              #72.86714

#part c
winetrain = subset(wine, wine$vintage <= 1981)
winemodel1 <- lm(log(price91)~age91,winetrain)
summary(winemodel1)                    #0.6675

#part d
confint(winemodel1,level=0.99)       #B0 ranges from 3.159 to 3.983 while B1 ranges from 0.022873 to 0.062347

#part e
winetest = subset(wine, wine$vintage > 1981)
winemodel1prediction = predict(winemodel1, winetest)
winessemodel <- sum((log(winetest$price91)-winemodel1prediction)^2)          #(true - predicted value)^2, and sum all up
winesstmodel <- sum((log(winetest$price91)-mean(log(winetrain$price91)))^2)     #use Ytrain. More mathematically valid
1 - (winessemodel/winesstmodel)    

#test R^2 = 0.9213742

#part f
#model R^2 and test R^2 much higher for this new dataset
#variation in the prices of wines explained much more by age compared to Bordeaux dataset 


#part g
winemodel2 <- lm(log(price91)~age91 + temp + hrain + wrain +tempdiff,winetrain)
summary(winemodel2)                          #R^2 = 0.7938

#part h 
#with all vars, adjusted R^2 increased from 0.65 to 0.7145, therefore latter model preferred

#part i
#lesser harvest rain, better price and quality of the wine due to negative correlation and significance (p<0.05)

#part j
winemodel3<- lm(log(price91)~age91 + temp +hrain, winetrain)
summary(winemodel3)                          #R^2 = 0.7753

#part k
#this model preferred since adjusted R^2 increased from 0.7145 to 0.7304

#part l
winemodel4<- lm(log(price92)~age92 + temp +hrain, winetrain)
summary(winemodel4)                         #R^2 = 0.5834

#part m
#since p value for hrain is 0.320 > 0.2, therefore not statistically significant that coefficient is nonzero. Don't reject.

#part n
#can check for consistency of effect of weather vars and age by looking at signs of estimated coeffs

#part o
#if too many missing entries, may lose a lot of data


#Question 7
#part a
batters = read.csv("batters.csv")
batters2006 = subset(batters, batters$yearID == 2006)
which.max(batters2006$salary)       #263
batters2006$playerID[263]           #giambja01

#part b
which.min(batters2006$salary)      #8
batters2006$salary[263]/batters2006$salary[8]      #ratio is 61.65413. top player makes 61.5413 times more than last player

#part c and d: which team has max amt of salaries, which team has min amt of salaries
batters$salary[batters$yearID==1996]       #all salaries for 1996
batters$teamID[batters$yearID==1996]       #all teams for 1996
tapply(batters$salary[batters$yearID==1996], batters$teamID[batters$yearID==1996],sum)       #for each team, find salary and sum all
sort(tapply(batters$salary[batters$yearID==1996], batters$teamID[batters$yearID==1996],sum))    #sort summed salaries from lowest to highest

#part e
hist(batters$salary)
#most salaries are small, with relatively small no of large salaries (right skewed)

#part f
#when handling skewed dependent var, often useful to predict log of dependent var
battersmodel1 <- lm(log(salary)~R,batters)
summary(battersmodel1)                            #log(salary) = 13.417505 + 0.014991(R)
predict(battersmodel1, data.frame(R= 0))                         #when R = 0, log(salary) = 13.417505

#part g
batters$salary[batters$R == 0]         #salaries of people with 0 runs
mean(log(batters$salary[batters$R == 0]),na.rm= TRUE)         #remove NA values, find mean salary
#13.60

#part h
#values are close to ea other. If only regress log(salary) with a constant, best fit wil be mean

#part i
#log(salary) = B0 + B1R
#salary = e^(B0 + B1R)
#when runs increase by 1, new salary = e^(B0 + B1(R+1)) = e^(B0 + B1R + B1) = e^(B0 + B1R) * e^B1 = old salary * e^B1

#part j 
batters$OBP = (batters$H + batters$BB + batters$HBP)/ (batters$AB + batters$BB + batters$HBP + batters$SF)
batters$SLG = (batters$H + batters$X2B + 2*batters$X3B+ 3* batters$HR)/ (batters$AB)
mean(batters$OBP[batters$yearID==2006],na.rm= TRUE)                 #avg OBP% is 0.2707986

#part k
t.test(batters$SLG[batters$yearID==1996],batters$SLG[batters$yearID==2006])
#p-value is 0.4045 > 0.05, therefore not sig difference, cannot reject null hypothesis that diff = 0. 

#part l
model2train <- subset(batters, AB >= 130 & yearID == 1996)
battersmodel2 <- lm(log(salary)~OBP + SLG, model2train)
summary(battersmodel2)                 #adjusted R^2 is 0.2589

#part m 
#since all p values < 0.05, can reject null hypotheses

#part n 
model3train <- subset(batters, AB >= 130 & yearID == 2006)
battersmodel3 <- lm(log(salary)~OBP + SLG, model3train)
summary(battersmodel3)                 #adjusted R^2 is 0.1164

#part o
#undervalued OBP, corrected in 2006, therefore OBP significance went up 