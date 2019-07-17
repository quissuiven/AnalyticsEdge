

#Question 1
#part a
Heating = read.csv("Heating.csv")
str(Heating)
summary(Heating)

library("mlogit")
#there are 5 diff alternatives. Right now each has its own operating cost and installation cost. Need to merge into 1
#still dun understand next step
data = mlogit.data(Heating, choice="depvar", shape = "wide", varying = c(3:12))    #identifying vars that are alternatives (the 5 ics, 5 ocs), then merge into 1 )
datatest1 = mlogit.data(Heating, choice="depvar", shape = "wide")         #each row is an observation (er, ec, gc ... can repeat)
#datatest2 = mlogit.data(Heating, choice="depvar", shape = "long")         #each row is an alternative, can repeat

logitmodel1 <- mlogit(depvar~ ic + oc , data)
summary(logitmodel1)

#part a(i)
#negative coefficients, since as OC and IC increases, probability of choosing sys goes down

#part a(ii)
#p-values very near 0, therefore can reject null hypothesis that coefficient v near 0

#part a(iii)
pred1 <- predict(logitmodel1, newdata = data)       #predict depvar using ic + oc in data
pred1
apply(pred1, 2, mean)     #computes average probability for depvar (avg predicted proportion)

table(data, pred1)

table(Heating$depvar)/900          #actual proportion of houses: out of 900, how many houses of certain category

#model captures data reasonably well, but still have differences

#part a(iv)
logitmodel1$coefficients[2]/logitmodel1$coefficients[1]      #0.7349453.   Reducing $1 operating cost, willing to pay 0.735 higher installation cost
#doesn't make sense to pay only 1 time fee of $0.73, but have $1 reduction in operating cost every year

#alternatively
#logitmodel1$coefficients["oc"]/logitmodel1$coefficients["ic"]


#part b
data$LCC <- data$ic + data$oc/0.12
logitmodel2 <- mlogit(depvar~ LCC -1, data)
summary(logitmodel2)         #log-likelihood: -1248.7

#part c
logitmodel3 <- mlogit(depvar~ ic + oc, data, reflevel = "hp")     #forces hp to be reference level, coefficient normalized to 0
                                                                  #including intercept introduces alternative specific constants
summary(logitmodel3)                                #negative log likelihood is -1008.2

#part c(i)
pred2 <- predict(logitmodel3, newdata = data)
apply(pred2, 2, mean)                           #avg predicted share of choosing ea alternative
table(Heating$depvar)/900                       #actual share of choosing ea alternative

#predicted and actual match exactly

#part c(ii)
logitmodel3$coefficients["oc"]/logitmodel3$coefficients["ic"]   #$4.56 on-time installation cost for $1 annual saving in operating costs, therefore reasonable

#part c(iii)
logitmodel4 <- mlogit(depvar~ ic + oc, data, reflevel = "gr")     #gr is reference level. Reduce alt specific constants/coeffs of other vars ec er, gc by 0.308
summary(logitmodel4)

#part d(i)
data$iic <- data$ic / data$income
logitmodel5 <- mlogit(depvar~ iic + oc, data)
summary(logitmodel5)                                #negative log likelihood is -1010.2, worst than c, since we want to max log likelihood
                                                    #iic no longer significant

#part d(ii)
logitmodel6 <- mlogit(depvar~ ic + oc| income, data)        #estimates coeff for ea alternative that is income-dependent
summary(logitmodel6)                                     #as income rises, p(choosing heat pump) rises, p(choosing gas room) falls. None of alternatives significant

#part e(i)
Heating1 <- Heating
Heating1$ic.hp <- 0.9 * Heating$ic.hp
data1 <- mlogit.data(Heating1, choice="depvar", shape = "wide", varying = c(3:12))
pred3a <- predict(logitmodel3, newdata = data1)
apply(pred3a, 2, mean)      #market share of hp increases to 6.45% from 5.5%, by introducing 10% rebate

#part e(ii)
#Creating 6th alternative
df <- subset(Heating, select = c(3:12))    #take only ic and oc of 5 original alternatives
df$ic.eci <- df$ic.ec + 200
df$oc.eci <- 0.75 * df$oc.ec      #new alternative eci has $200 more ic and 75% oc

#Using model 3 coefficients to calculate new probabilities after adding 1 more alternative. Based on eqn
logitmodel3$coef    

df$hpexp <- exp(logitmodel3$coef["oc"]*df$oc.hp + logitmodel3$coef["ic"]*df$ic.hp)          #using model where hp is ref level
df$ecexp <- exp(logitmodel3$coef["oc"]*df$oc.ec + logitmodel3$coef["ic"]*df$ic.ec + logitmodel3$coef["ec:(intercept)"])   #e^ alternative specific constants and coeffs * X
df$erexp <- exp(logitmodel3$coef["oc"]*df$oc.er + logitmodel3$coef["ic"]*df$ic.er + logitmodel3$coef["er:(intercept)"])
df$gcexp <- exp(logitmodel3$coef["oc"]*df$oc.gc + logitmodel3$coef["ic"]*df$ic.gc + logitmodel3$coef["gc:(intercept)"])
df$grexp <- exp(logitmodel3$coef["oc"]*df$oc.gr + logitmodel3$coef["ic"]*df$ic.gr + logitmodel3$coef["gr:(intercept)"])
df$eciexp <- exp(logitmodel3$coef["oc"]*df$oc.eci + logitmodel3$coef["ic"]*df$ic.eci + logitmodel3$coef["ec:(intercept)"])

#normalize by sum of exponentials to create probabilities
df$sumexp <- apply(subset(df,select=c(13:17)),1,sum)                #create subset of cols (hpexp, ecexp... grexp), sum over row
df$sumnewexp <- apply(subset(df,select=c(13:18)),1,sum)             #create subset of cols (hpexp, ecexp... eciexp), sum over row

df$hp <- df$hpexp/ df$sumexp           #P(hp)
df$ec <- df$ecexp/ df$sumexp
df$er <- df$erexp/ df$sumexp
df$gc <- df$gcexp/ df$sumexp
df$gr <- df$grexp/ df$sumexp

df$hpnew <- df$hpexp/ df$sumnewexp        #P(hpnew)
df$ecnew <- df$ecexp/ df$sumnewexp
df$ernew <- df$erexp/ df$sumnewexp
df$gcnew <- df$gcexp/ df$sumnewexp
df$grnew <- df$grexp/ df$sumnewexp
df$ecinew <- df$eciexp/ df$sumnewexp

df2 <- subset(df, select = c(26:31))     #create subset of cols (hpnew, ecnew... ecinew)

mktsharenew <- apply(df2,2,mean)         #take mean of col to get avg probability of ea alternative
mktshareold <- apply(predict(logitmodel3, data),2,mean)


#gc 0.63 -> gcnew 0.57 (w 6 alternatives)




#Question 2
#part a (i)
electricity = read.csv("Electricity.csv")
str(electricity)                #4308 obs
summary(electricity)
library(mlogit)
Elecdata = mlogit.data(electricity, id.var = "id", choice="choice", shape = "wide", varying = c(3:26), sep = "")   #id.var: name of var that contains individual index, sep: separator of var name and alt name (separate pf1 pf2 pf3 pf4 into pf)
Elecdata

Elecmodel1 = mlogit(choice~ pf + cl + loc + wk + tod + seas -1, Elecdata, rpar = c(pf = "n"))   #changing pf to normal
summary(Elecmodel1)

Elecmodel2 = mlogit(choice~ pf + cl + loc + wk + tod + seas -1, Elecdata, rpar = c(pf = "n", cl = "n", loc ="n", wk ="n", tod ="n", seas = "n"))   #changing all 6 alts to normal
summary(Elecmodel2)

Elecmodel3 = mlogit(choice~ pf + cl + loc + wk + tod + seas -1, Elecdata, rpar = c(pf = "n", cl = "n", loc ="n", wk ="n", tod ="n", seas = "n"), panel = TRUE)   #with panel data: multiple obs per individual 
summary(Elecmodel3)

#mean coeff of cl (contract length) is -0.18, mean coeff of price is -0.84
#willingness to pay for unit increase in cl = -0.18/-0.84 = 21 cents per KWh

#part a(ii)
#share of population who dislike long term contracts

#table(Elecdata$cl)/4308           #normal distribution diff way of calculating

#coeff of length is normally distributed with mean -0.18 and standard deviation 0.31
#share of ppl w negative coefficients = P(mean + sd * N(0,1)) < 0 = pnorm(-model1$coef["cl"]/model1$coef["sd.cl"])
pnorm(-Elecmodel3$coef["cl"]/Elecmodel3$coef["sd.cl"])    #0.719 => 72% of population dislike long term contracts. Only 28% have positive price coefficient

#part b

pnorm(Elecmodel3$coef["pf"]/Elecmodel3$coef["sd.pf"])   #1.83677e-07 of population has positive price coefficient

Elecmodel4 = mlogit(choice~ pf + cl + loc + wk + tod + seas -1, Elecdata, rpar = c(cl = "n", loc ="n", wk ="n", tod ="n", seas = "n"), panel = TRUE)   #make price fixed, instead of vary based on normal dist
summary(Elecmodel4)
#new log likelihood is -4110.2, more negative than before. Estimated coeff for pf is -0.81

#part c
Elecmodel5 = mlogit(choice~ pf + cl + loc + wk + tod + seas -1, Elecdata, rpar = c(cl = "n", loc ="n", wk ="u", tod ="n", seas = "n"), panel = TRUE)   #make price fixed, instead of vary based on normal dist
summary(Elecmodel5)
#uniform dist of wk is from 0.133 to 2.58 with mean = 1.36, estimated coeff for pf is -0.81



#Question 3
#part a 
#for every value of k, best subset selection has lowest training sum of squared error. This finds the global optimum level for ea

#part b
#This cannot be determined since low training sum of squared error does not necessarily translate to low test sum of squared error

#part c
#i: TRUE
#ii: TRUE      #talking k var and k+1 var not iteration
#iii: FALSE    #backward and forward are independent, we dunno whether subset
#iV: FALSE
#v: FALSE      #since some vars can enter and leave


#Question 4
#part a
college = read.csv("College.csv")            #777 obs

set.seed(1)

trainid <- sample(1:nrow(college),nrow(college)*0.80)       
testid <- -trainid

train <- college[trainid,]           #why need this step? Why cannot just use trainid
test <- college[testid,]

str(train)      #621 obs
str(test)       #156 obs

#part b
collegemodel1 <- lm(Apps~., data= train)
summary(collegemodel1)
sum((collegemodel1$residual)^2)        #total sum of squared error
mean((collegemodel1$residual)^2)        #average sum of squared error = 1061946

predict1 <- predict(collegemodel1, newdata = test)
msetest1 <- mean((test$App - predict1)^2)       #average sum of squared error = 1075064

#part c
library(leaps)
collegemodel2 <- regsubsets(Apps~., data= train, nvmax=17, method = "backward")
summary(collegemodel2)        #subset of size 16, has P.undergrad dropped first

#part d
plot(summary(collegemodel2)$adjr2)
which.max(summary(collegemodel2)$adjr2)    #subset of size 12 has max adjr2
coef(collegemodel2,12)                     #PrivateYes, Accept, enroll, top10perc, top25perc, F.undergrad, Outstate...  
                                
#part e
collegemodel3 <- lm(Apps~ Private + Accept + Enroll + Top10perc + Top25perc + F.Undergrad + Outstate + Room.Board + PhD + S.F.Ratio + Expend + Grad.Rate, data=train)
summary(collegemodel3)
predict2 <- predict(collegemodel3, newdata = test)
msetest2 <- mean((test$App - predict2)^2)      #average sum of squared error = 1070293, decreased, therefore accuracy improves

#part f
library(glmnet)
grid <- 10^seq(10,-2,length=100)
x <- model.matrix(Apps~., college)
y <- college$Apps
collegemodel4 <- glmnet(x[trainid,], y[trainid], lambda = grid)
plot(collegemodel4, xvar = "lambda")

#part g
set.seed(1)
collegemodel5 <- cv.glmnet(x[trainid,], y[trainid], lambda = grid)    #default is 10 folds
collegemodel5$lambda.min       #0.4977024
predictcollegecv <- predict(collegemodel5, s= 0.4977024, newx = x[testid,])    #predict using most optimal lambda
mean((predictcollegecv - y[testid])^2)           #1075261, similar to collegemodel1

collegemodel5$nzero    #17 non-zero coeff estimates
