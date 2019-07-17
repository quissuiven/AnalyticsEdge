
#Question 1
#part a:  LOGISTIC REGRESSION MODEL
census = read.csv("census.csv")

str(census)          #31978 obs
library(caTools)
set.seed(2000)
censusspl <- sample.split(census$over50k, SplitRatio = 0.6)       #why census$50k, not census?   By any column
censustrain <- subset(census, censusspl == TRUE)      #retrieve obs where censusspl == TRUE
censustest <- subset(census, censusspl == FALSE)

str(censustrain)             #19187 obs
str(censustest)

model1 <- glm(over50k~., data = censustrain, family = binomial)
summary(model1)                #many factor vars significant at 0.01


#part b
predict1 <- predict(model1, newdata= censustest, type ="response")
predict1
table(predict1>= 0.5, censustest$over50k)
9051 + 1888/ 9051 + 1888 + 1190 + 662         #85.5% test accuracy

#part c
table(censustest$over50k)
3078/ 9713 + 3078                       #24.1% baseline accuracy

#part d
library(ROCR)

predictcensuslog <- prediction(predict1, censustest$over50k)
performance(predictcensuslog, measure = "auc")            #AUC is 0.9061598

#part e: CLASSIFICATION TREE 
library(rpart)
library(rpart.plot)
cartmodel1 <- rpart(as.factor(over50k)~., data = censustrain)
printcp(cartmodel1) 
prp(cartmodel1)

#part f
#4 splits

#part g
cartmodel1

#first split is relationship

#part h

#second split vars are capital gain < 7566 and education = 10th, 11th, 12th, 1st-4th, 5th-6th, 7th-8th, 9th, Assoc-acdm, Assoc-voc, HS-grad, Preschool, Some-college 6158 2077  <=50

#part i
cartpredict1 <- predict(cartmodel1, newdata= censustest)
cartpredict1
table(cartpredict1[,2]>= 0.5, censustest$over50k)
9243 + 1596/ 9243 + 1596 + 1482 + 470           #test accuracy is 0.84739269799

#part j
predictcartlog <- prediction(cartpredict1[,2], censustest$over50k)           #[,2] necessary to get values of 1
cartperf <- performance(predictcartlog, measure = "tpr", x.measure ="fpr")
plot(cartperf)         #plotting ROC curve for cart model

logitperf <- performance(predictcensuslog, measure = "tpr", x.measure ="fpr")
plot(logitperf)        #plotting ROC curve for logistic model

#ROC curve for cart model less smooth. 
#No of vars used in logistic regression larger than no of vars used in cart model (the smoothness comprises many rough edges like cart)

#part k
performance(predictcartlog, measure = "auc")             #AUC is 0.8470256

#part l: RANDOM FOREST MODEL
set.seed(1)
trainSmall <- censustrain[sample(nrow(censustrain), 2000),]  #downsample training set, since random forest computationally intensive
library(randomForest)
randfmodel1 <- randomForest(as.factor(over50k)~., data = trainSmall)
summary(randfmodel1)
randfpredict1 <- predict (randfmodel1, newdata= censustest)    #don't need type = "class" if want to use threshold of 0.5
#table(randfpredict1 >= 0.5, censustest$over50k)       #not meaningful for factors
table(randfpredict1, censustest$over50k)  

#part m 
#random forest models work by building large collection of trees, 
#therefore lose some interpretability that comes w CART 
#in terms of seeing how predictions made, and which variables important
#however, can still compute metrics that give insight on which variables important
#eg. number of times a certain variable is selected for a split 
vu <- varUsed(randfmodel1, count = TRUE)    
vusorted <- sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(randfmodel1$forest$xlevels[vusorted$ix]))   #measures no of times ea var selected for splitting
#vars age, hoursperweek, occupation, education, workclass split the most times, most impt

#part n
#another metric is impurity
#one way to measure the importance of a var is to  average the reduction in impurity, taken over all the times that var is selected for splitting
varImpPlot(randfmodel1)      #occupation, education, age, relationship, capitalgain, maritalstatus, hoursperweek decreased in purity the most

#part o: how CART behaves w diff choices of its parameters
library(rpart)
library
cartmodel2 <- rpart(over50k~.,data = censustrain, cp = 0.0001)
#using kfold to determine cost complexity parameter. For CART, can use printcp
printcp(cartmodel2)                      #smallest xerror is split = 14, cp = 0.001   
plotcp(cartmodel2)
#modify cost complexity parameter
cartmodel3 <- prune(cartmodel2, cp = 0.001)           #modify based on optimal cp 
prp(cartmodel3)

#part p
cartpredict2 <- predict(cartmodel2, newdata= censustest)
cartpredict2
table(cartpredict2[,2]>= 0.5, censustest$over50k)    

#question 2: fitting regression trees to Boston dataset
#part a
boston = read.csv("Boston.csv")
str(boston)
library(caTools)
set.seed(1)
trainID <- sample(1:nrow(boston), nrow(boston)/2)               #use sample instead of sample.split
bostontrain <- boston[trainID,]                          #include all columns
bostontest <- boston[-trainID,]  
str(bostontrain)       #253 obs
str(bostontest)        #253 obs
library(rpart)
library(rpart.plot)
regressmodel1 <- rpart(medv~., data = bostontrain)       #regression tree since medv numeric type
prp(regressmodel1)            #3 predictor vars used: lstat, rm, dis

#part b
regresspredict <- predict(regressmodel1, newdata = bostontest)
mse <- mean((regresspredict - bostontest$medv)^2)
mse         #25.35825
plot(regresspredict, bostontest$medv)
abline(1:50, 1:50)
sqrt(mse)     #predictions are within 5.035 of the true value

#part c
printcp(regressmodel1)       #prints result from cross validation
prp(regressmodel1)

#smallest xerror happens when split  = 7, cp = 0.01
#therefore will not prune the tree developed earlier

#part d
regressmodel2 <- prune(regressmodel1, cp = 0.02169)           #prune using 5 splits
prp(regressmodel2)
regresspredict2 <- predict(regressmodel2, newdata = bostontest)
mse2 <- mean((regresspredict2 - bostontest$medv)^2)
mse2             #28.25719, mean squared error higher

#part e: RANDOM FOREST
library(randomForest)
set.seed(1)
bostonrandfmodel1 <- randomForest(medv~.,data= bostontrain)
randfpredict1 <- predict(bostonrandfmodel1, newdata= bostontest)
mse3<- mean((randfpredict1 - bostontest$medv)^2)
mse3         #11.61762, test mean squared error significantly lower than CART, tries 4 vars by default

#part f
importance(bostonrandfmodel1)          #to determine vars which are impt. Highest purity are rm and lstat
varImpPlot(bostonrandfmodel1)

#part g
#by default random forests use p/3 vars in regression, sqrt(p) vars in classification ,   where p is no of vars to randomly sample
# here 13/3 = 4
#setting mtry = 6, more variables at ea split of tree, increases test mse. This is becauses trees become highly correlated, prediction power worsens

#question 3
#part a
supreme = read.csv("supremeexercise.csv")
table(supreme$result, supreme$lctdir)    #no of cases reversed = 170 + 198/ 170 + 198 + 94 + 136   = 0.6153

#part b
head(supreme)
supreme[,5:13]
rowSums(supreme[,5:13])    #add up numbers in columns 5 to 13
supreme$unCons <- as.integer(rowSums(supreme[,5:13]) == 9)                      #if the sum is 9, then unanimous conservative decision
table(supreme$unCons)           #143 cases unanimous conservative decisions, 455 other decisions

#part c
supreme$unLib <- as.integer(rowSums(supreme[,5:13]) == 0)                      #if the sum is 0, then unanimous liberal decision
table(supreme$unLib)            #124 cases unanimous liberal decisions

#part d
library(rpart)
library(rpart.plot)
cartmodel4 <- rpart(as.factor(unCons)~petit + respon + circuit + unconst + lctdir + issue, data = supreme)
printcp(cartmodel4)   #total no of splits = 7

#part e
prp(cartmodel4)       #split on circuit, issue, petit, respon

#part f
library(ROCR)
cartpredict4 <-  predict(cartmodel4, newdata = supreme)
cartmodel4prediction <- prediction(cartpredict4[,2], supreme$unCons)
performance(cartmodel4prediction, measure ="auc")    #0.6519788

#part g
cartmodel5 <- rpart(as.factor(unLib)~petit + respon + circuit + unconst + lctdir + issue, data = supreme)
prp(cartmodel5)   #1st var split on is respon

#part h
prp(cartmodel5, extra = 1)    #plots no of observations in ea node. Leaf node w fewest number has 11 observations, output = 0
                              #fraction of with unanimous liberal decision made at this node = 3/11

#part i
conservativepredict <- predict(cartmodel4, newdata = supreme, type = "class")
liberalpredict <- predict(cartmodel5, newdata = supreme, type = "class")
table(conservativepredict, liberalpredict)    #2 cases unanimous conservative or liberal


#part j
#502 cases neither predict unanimous


#part k
supreme1 <- subset(supreme,conservativepredict==liberalpredict)
str(supreme1)
model3 <- rpart(as.factor(rehndir)~petit+respon+circuit+unconst+lctdir+issue,data=supreme1)
model4 <- rpart(as.factor(stevdir)~petit+respon+circuit+unconst+lctdir+issue,data=supreme1)
model5 <- rpart(as.factor(ocondir)~petit+respon+circuit+unconst+lctdir+issue,data=supreme1)
model6 <- rpart(as.factor(scaldir)~petit+respon+circuit+unconst+lctdir+issue,data=supreme1)
model7 <- rpart(as.factor(kendir)~petit+respon+circuit+unconst+lctdir+issue,data=supreme1)
model8 <- rpart(as.factor(soutdir)~petit+respon+circuit+unconst+lctdir+issue,data=supreme1)
model9 <- rpart(as.factor(thomdir)~petit+respon+circuit+unconst+lctdir+issue,data=supreme1)
model10 <- rpart(as.factor(gindir)~petit+respon+circuit+unconst+lctdir+issue,data=supreme1)
model11 <- rpart(as.factor(brydir)~petit+respon+circuit+unconst+lctdir+issue,data=supreme1)
predict3 <- predict(model3,newdata=supreme1,type="class")
predict4 <- predict(model4,newdata=supreme1,type="class")
predict5 <- predict(model5,newdata=supreme1,type="class")
predict6 <- predict(model6,newdata=supreme1,type="class")
predict7 <- predict(model7,newdata=supreme1,type="class")
predict8 <- predict(model8,newdata=supreme1,type="class")
predict9 <- predict(model9,newdata=supreme1,type="class")
predict10 <- predict(model10,newdata=supreme1,type="class")
predict11 <- predict(model11,newdata=supreme1,type="class")
totalcons <-  as.numeric(as.character(predict3))+ as.numeric(as.character(predict4))+ as.numeric(as.character(predict5))+as.numeric(as.character(predict6))+ as.numeric(as.character(predict7))+ as.numeric(as.character(predict8))+ as.numeric(as.character(predict9))+ as.numeric(as.character(predict10))+ as.numeric(as.character(predict11))
table(totalcons>=5,supreme1$result)


#part m
prp(cartmodel4)        #predicts unanimous conservative decision. At top node, uncons = 0
prp(cartmodel5)        #predicts unanimous liberal decision. 


#part n
library(randomForest)
set.seed(1)
randforestmodel1 <- randomForest(as.factor(result)~petit + respon + circuit + unconst+ lctdir + issue, data = supreme)
randforestpredict <- predict(randforestmodel1, newdata= supreme)
table(randforestpredict, supreme$result)   #229 + 308/ 229 + 308 + 26 + 35 = 0.899

#part o
#CART is more interpretable, less accuracy. While RF is less interpretable, more accuracy.
