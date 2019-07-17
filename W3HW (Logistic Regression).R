
#Question 1
#part a

baseballlarge = read.csv("baseballlarge.csv")
str(baseballlarge)                    #1232 observations
head(baseballlarge)
View(baseballlarge)
baseballlarge$Team

table(baseballlarge$Year)             #display no for diff years
length(table(baseballlarge$Year))     #count no of years

baseballlarge <- subset(baseballlarge,baseballlarge$Playoffs==1)     #subset teams that made to playoffs
str(baseballlarge)                    #244 obs

table(baseballlarge$Year)
table(table(baseballlarge$Year))        #7 years which had 2 teams in playoffs, 23 years which had 4 teams in playoffs, 16 years which had 8


#part b
table(baseballlarge$Year)          
#table(baseballlarge$Year)[baseballlarge$Year]      #doesn't work since Year is an int.
table(baseballlarge$Year)[as.character(1962)]       #retrieve count for year 1962
table(baseballlarge$Year)[as.character(baseballlarge$Year)]        #retrieve count for each year
baseballlarge$NumCompetitors <- table(baseballlarge$Year)[as.character(baseballlarge$Year)]

table(baseballlarge$NumCompetitors)           #128 obs where 8 teams were invited to playoffs


#part c
str(baseballlarge$RankPlayoffs)      #int, containing  1,2,3,4,5. If 1, return 1. Otherwise return 0.
baseballlarge$RankPlayoffs==1        #returns true/false, therefore need as.integer. Does not subset all the 1s, but checks whether == 1.
baseballlarge$WorldSeries <- as.integer(baseballlarge$RankPlayoffs==1)
baseballlarge$WorldSeries
table(baseballlarge$WorldSeries)     #197 obs that did not win

#part d  building simple models
model1 <- glm(WorldSeries~ Year, data = baseballlarge, family = binomial)       #2 stars
summary(model1)

model2 <- glm(WorldSeries~ RS, data = baseballlarge, family = binomial)         #0 stars
summary(model2) 

model3 <- glm(WorldSeries~ RA, data = baseballlarge, family = binomial)         #1 star
summary(model3) 

model4 <- glm(WorldSeries~ W, data = baseballlarge, family = binomial)          #0 stars
summary(model4) 

model5 <- glm(WorldSeries~ OBP, data = baseballlarge, family = binomial)          #0 stars
summary(model5) 

model6 <- glm(WorldSeries~ SLG, data = baseballlarge, family = binomial)          #0 stars
summary(model6) 

model7 <- glm(WorldSeries~ BA, data = baseballlarge, family = binomial)          #0 stars
summary(model7) 

model8 <- glm(WorldSeries~ RankSeason, data = baseballlarge, family = binomial)          #1 star
summary(model8) 

model9 <- glm(WorldSeries~ NumCompetitors, data = baseballlarge, family = binomial)          #3 stars
summary(model9)

baseballlarge$League == "AL"
baseballlarge$League <- as.integer(baseballlarge$League == "AL")
model10 <- glm(WorldSeries~ League, data = baseballlarge, family = binomial)          #0 stars
summary(model10)

#Year, RA, RankSeason and NumCompetitors are significant vars

#part e
model11 <- glm(WorldSeries~ Year + RA + RankSeason + NumCompetitors, data = baseballlarge, family = binomial)
summary(model11)
#none of the vars are significant

#part f  use cor() function
cor(baseballlarge[,c("Year","RA","RankSeason","NumCompetitors")])        #Year and NumCompetitors have high correlation

#part g
model12 <- glm(WorldSeries~ Year + RA, data = baseballlarge, family = binomial)       #AIC 233.88
summary(model12)

model13 <- glm(WorldSeries~ Year + RankSeason, data = baseballlarge, family = binomial)  #AIC 233.55
summary(model13)

model14 <- glm(WorldSeries~ Year + NumCompetitors, data = baseballlarge, family = binomial)   #AIC 232.9
summary(model14)
 
model15 <- glm(WorldSeries~ RA + RankSeason, data = baseballlarge, family = binomial)         #AIC 238.22
summary(model15)

model16 <- glm(WorldSeries~ RA + NumCompetitors, data = baseballlarge, family = binomial)     #AIC 232.74
summary(model16)

model17 <- glm(WorldSeries~ RankSeason + NumCompetitors, data = baseballlarge, family = binomial)     #AIC 232.52
summary(model17)

#best AIC value is model using NumCompetitors
#seems like winning playoffs large role of luck


#question 2
#part a
Parole = read.csv("Parole.csv")            #675 obs

#part b
table(Parole$Violator)                  #78 parolees who violated

#part c
#Unordered factors w at least 3 levels:  State (Kentucky, Louisiana, Other, Virginia), Crime(Driving, Drugs, Larceny, Other)

#part d
set.seed(144)
library(caTools)
split <- sample.split(Parole$Violator, SplitRatio = 0.7)
train <- subset(Parole, split == TRUE)
test <- subset(Parole, split == FALSE)
#first run: train has 473 obs, test has 202 obs
#exact same training/testing set split since set seed to be the same

#if just run 3-5, diff training/testing set split
#if set diff no, and run 3-5, diff training/testing set split

#part e
model1 <- glm(Violator~.,data= train, family = binomial)     #sig vars are StateVirginia, MultipleOffenses

#part f
#MultipleOffenses coeff B = 1.511263.
#Odds of parolee to be violator compared to non-violator = P(Y=1)/ P(Y=0) = e^1.51 = 5.01

#part g
#Odds = e^(-2.03 + 0.38(1)+ 0.88(1) - 0.0017(50) - 0.12(3)) + 0.08(12) + 1.66(0) + 0.695(1)), substitute in x-values
#logOdds = -2.03 + 0.38(1)+ 0.88(1) - 0.0017(50) - 0.12(3)) + 0.08(12) + 1.66(0) + 0.695(1)
logOdds = model1$coef[1] + model1$coef[2]*1 + model1$coef[3]*1 + model1$coef[4] * 50 + model1$coef[8]*3 + model1$coef[9]*12 + model1$coef[10]*0 + model1$coef[12]*1
Odds = exp(logOdds)   #Odds that individual is violator = 0.28

#part h
pred <- predict(model1,newdata = test, type = "response")  
                        #for a default binomial model the default predictions are of log-odds (probabilities on logit scale) 
                        #and type = "response" gives the predicted probabilities
summary(pred)    #max is 0.907279 

#part i
table(pred>0.5)
table(as.integer(pred>0.5),test$Violator)            #further separate TRUE FALSE values, based on Violate or No violate  
table(paroleetest>0.3)


# accuracy = TN + TP/ TN + TP + FN + FP =  167 + 12/ 167 + 11 + 12 + 12
# false positive rate = FP/ FP + TN         
# sensitivity = true positive rate = TP/ TP + FN    =     12/(12 + 12)   
# specificity = true negative rate = TN/ TN + FP    =     167/(167 + 11)

#part j
#by right. False = 179, True = 23. Accuracy = 179/ 179 + 23
table(test$Violator)

#part k 
#there will be problem if release people who will violate parole = false negatives, there should assign more cost to false negatives than false positives
#should also use a cutoff < 0.5, makes more ppl to be predicted as positive, reducing undesirable outcomes

#part l
table(test$Violator)                        #simple baseline model             => 11 false negatives (or is false positives?)
table(as.integer(pred>0.5),test$Violator)      #logistic regression model         => 23 false negatives
#therefore logistic regression model adds value. Changing threshold is likely to improve model's value

#part m 
library(ROCR)
predROCR <- prediction(pred, test$Violator)
auc <- performance(predROCR, measure = "auc")       #0.8945834, 

#part n
#auc greater means true positive greater than false positive
#therefore auc can be interpreted as probability that model can correctly differentiate between randomly selected
#parole violator and a randomly selected parole non-violator

#part o
#dataset contains individuals released from parole due to completing parole term or violating
#but does not contain those who neither violate parole or not complete parole term
#Option 2, of putting Violator ==0 doesn't help, since just means haven't violated/ completed parole term
#therefore shld track when violate/ complete parole term



#Question 3
#want to dvlp a model to determine if new applicants present a good credit risk or bad credit risk
#resp var: credit rating is good. 0 or 1
#part a
germancredit = read.csv("germancredit.csv")
set.seed(2016)
library(caTools)
spl <- sample.split(germancredit$resp,0.75)         #balances dependent var for test set and training, same ratio

training <- subset(germancredit, spl == TRUE)     #split data frame into training data set, for which spl is TRUE
test <- subset(germancredit, spl == FALSE)        #split data frame into testing data set, for which spl is FALSE

table(training$resp)       #ratio is 0.7 for both training and test
table(test$resp)

#part b
model1 <- glm(resp~1, data= training, family = binomial)     
summary(model1)

#part c
#P(Y=1) = e^(0.84730)/ 1 + e^(0.84730)

#part d
model2 <- glm(resp~., data= training, family = binomial)
summary(model2)      #AIC = 751.55
#significant vars are chkacct, dur, hist, amt, sav,instrate, malesingle, age, for.

#part e: calculate log likelihood
#from model, residual deviance = 689.55
#Residual deviance = -2(LL) = 689.55   =>   LL = -344

#part f
testpred <- predict(model2, newdata = test, type = "response")
table(testpred>0.5)                      #FALSE TRUE is predicted
table(testpred>0.5, test$resp)
table(as.integer(testpred>0.5), test$resp)     #as.integer makes it 0 or 1

#part g
# Accuracy = 40 + 157/ 40 + 157 + 18 + 35 = 0.788

#part h
model3 <- glm(resp~ chkacct + dur + hist + amt + sav + instrate + malesingle + age + for.-1, data=training, family = binomial)
summary(model3)       #AIC = 752.19

#part i 
#model2 AIC is 751.55, model3 AIC is 750.24.  Therefore model3 AIC is lower and fits better.

#part j
testpred2 <- predict(model3, newdata = test, type = "response")
table(testpred2>0.5, test$resp)     #Accuracy = 33 + 160/ 33 + 160 + 15 + 42= 0.772 (therefore lower accuracy)

#part k
#False positive rate = FP/ FP + TN      predict good risk, when bad risk. lower better.
# for model2, 35/ 35 + 40 = 0.4667
# for model3, 42/ 42 + 33 = 0.56
# model2 better

#part l
#False negative rate = FN/ FN + TP      predict bad risk, when actually good risk. 
# for model2, 18/ 18 + 157 = 0.102
# for model3, 14/ 14 + 160 = 0.0857
# model3 better

#part m
library(ROCR)
ROCRpredmodel2 <- prediction(testpred, test$resp)
ROCRperfmodel2 <- performance(ROCRpredmodel2 , measure ="auc")     #auc for model2 is 0.8292571
ROCRperfmodel2

ROCRpredmodel3 <- prediction(testpred2, test$resp)
ROCRperfmodel3 <- performance(ROCRpredmodel3 , measure ="auc")     #auc for model3 is 0.782781
ROCRperfmodel3

#since model2 has higher auc, more preferable

#part n
table(testpred>0.5, test$resp)     #since using model2
#profit incurred is -300(35) +100(157) = 5200DM 

#part o
sortpred <- sort(testpred,decreasing = TRUE)        #last entry is 819 with probability of 0.0253 for good risk. 
sortpred
germancredit[819,]                                 #For this individual, duration of credit is 36 months


#part p
#for each obs in sorted test set, calculate actual profit
sortpred <- sort(testpred,decreasing = TRUE, index.return = TRUE)
sortpred

sortpred$x  #gives sorted values
sortpred$ix #gives indices of sorted values

test$resp[sortpred$ix]     #true positive

profitpred <- 100*(test$resp[sortpred$ix])-300*(1-test$resp[sortpred$ix])
profitpred

cumulative <- cumsum(profitpred)      #sum all previous values
cumulative

which.max(cumulative)    #position 150 has max. Go down to 150 people out of 250, for max profit
max(cumulative)         #7800 


#part q
sortpred[which.max(cumulative)]




#question 4
#part a
pres <- read.csv("presidential.csv")
table(pres$WIN)   #1 refers to democratic party win (14), -1 refers to republic party win (11)

#part b
table(pres$DEM)   #Roosevelt represented 4 times
table(pres$REP)   #Nixon represented 3 times

#part c 
pres$GOOD[pres$INC==-1]    #shows number of good quarters for Republican
pres$GOOD[pres$INC==1] 
t.test(pres$GOOD[pres$INC==-1],pres$GOOD[pres$INC==1]) 
          #null hypothesis:  GOOD when pres is Republican = GOOD when president is Democratic
          #alt hypothesis:                                !=
          #p-value is 0.7494 > 0.05. Therefore cannot reject null hypothesis, that no of good quarters is the same

#part d
pres$WININC <- as.integer(pres$INC== pres$WIN)
#pres$WININC <- ifelse(identical(pres$INC, pres$WIN),1,0)        #this does not work. No function to compare whether equal.

#part e
table(pres$WININC)            #16 wins, 9 loses

#part f
model4 <- glm(WININC~ GROWTH, data = pres, family= binomial)
summary(model4)
#Residual Deviance = -2(LL) = 26.365   =>           LL = -13.1825
#Alternative:   AIc = -2(LL) + 2(no of parameters) = 30.365       =>   LL = 30.365 -2(2)/ -2 = -13.185

#part g
#Significant at 0.1 level

#part h 
pres$WIN
pres$WIN <- ifelse(pres$WIN == 1,1,0)
#pres$WIN <- as.integer(pres$WIN ==1)       #check whether ea value is 1. If yes, 1.  If no, 0.
pres$GROWTH <- pres$GROWTH * pres$INC             #multiply by -1 for republican. If growth is negative, becomes positive.

#part i
pres$GOOD <- pres$GOOD* pres$INC  
model5 <- glm(WIN~ INC + RUN + DUR + GROWTH + GOOD, data = pres, family = binomial)
summary(model5)    #AIC = 29.406

#part j 
#3 least significant vars are INC, GOOD, INTERCEPT

#part k
model6 <- glm(WIN~  RUN + DUR + GROWTH - 1, data = pres, family = binomial)
summary(model6)    #AIC = 23.748,  becomes lower

#part l
#p-value for DUR is 0.1007, which is >0.10, therefore not significant

#part m 
#prefer model6, due to lower AIC

#part n
#INC = 1 (since democrats are in power)
#RUN = 0 (since obama did not run)
#DUR = 1 (since democrats have been in power for 2 consecutive terms)

#part o
#GROWTH = 2
#P(Dem =1) = e^(2.0638)(0)+(-1.7852)(1)+(0.4690)(2)/   1 +  e^(2.0638)(0)+(-1.7852)(1)+(0.4690)(2)  = 0.3
#P(Rep =1) = 1 - 0.3 = 0.7

