
#Classification and regression trees (CART)
#Is it impossible to predict whether the supreme court judges will affirm or reverse the lower court decision?
#(at the individual judge level, at the overall case level)

supreme <- read.csv("Supreme.csv")
head(supreme)
#docket: case number
#rehndir to brydir, decisions made by individual judges. 1 is conservative vote, 0 is liberal vote, 9 is not available
#supreme court result = 1 (conservative) or 0 (liberal), as opposed to lctdir (liberal or conservative)
#use 6 vars to predict individual judge decisions + result

stevens <- subset(supreme[,c("docket","term","stevdir", "petit", "respon", "circuit", "unconst", "lctdir", "issue")], supreme$stevdir != 9)    #picking out columns w corresponding var name, drop decisions that have value 9
head(stevens)

stevens$rev <- as.integer(stevens$stevdir == 0 & stevens$lctdir == "conser" | stevens$stevdir == 1 & stevens$lctdir == "liberal")        #if steven reverse decision: steven make liberal but lower court conservative or the converse. If reverse, give 1
head(stevens)
table(stevens$rev)   #278 cases affirm, 341 cases reverse

#Regression trees: for binary vars
#split entire space into partitions, 
# classification: predicts mode (occurs the most) for each partition
# regression: predicts average for each partition
#overfitting if too many partitions, eg. each individual prediction has a partition

#how to find cuts
#partition created so that most values in a partition are as homogeneous as possible (similar y outputs), but don't want so granular that overfit so need balance

#Adv of CART: 
#1. model extremely interpretable (if higher than blood pressure, go here. If above body fat percentage, go here)
#2. model does not assume linear relationship btw i/p and o/p, therefore captures nonlinearities (but does not do as well for linear relationships)
#   therefore depends on type of data

library(caTools)
set.seed(1)
spl <- sample.split(stevens$rev, SplitRatio = 0.7)   #since want 70% in training set, 30% in test set
train <- subset(stevens, spl == TRUE)
test <- subset(stevens, spl == FALSE)
str(train)
str(test)

m1 <- glm(rev~ petit + respon + circuit + unconst + lctdir + issue, family = binomial, data = train)
summary(m1)
p1 <- predict(m1, newdata= test, type ="response")       #error. Need to see values in training set to predict values in test set
table(train$issue)           #0 values for IR, 1 value for IR (doesn't see IR in training set)
table(test$issue)  

#drop IR, since 1 observation, insignificant

test <- subset(test, test$issue!="IR")
str(test)    #184 obs now
p1 <- predict(m1, newdata= test, type ="response")
table(p1>= 0.5, test$rev)
123/184               #70 times reverse and judge reverse.  accuracy is 0.6684783 or 66%   

#use repart (Reverse partitioning) to create trees
library(rpart)
library(rpart.plot)

cart1 <- rpart(rev~ petit + respon + circuit + unconst + lctdir + issue, data = train)         #returns MSE, doesn't make sense
summary(cart1)

#rpart will run a regression tree if the response variable is numeric, and a classification tree if it is a factor
cart2 <- rpart(as.factor(rev)~ petit + respon + circuit + unconst + lctdir + issue, data = train) #as factor var, fits as classification tree. Can add "method = class"
summary(cart2)
cart2              #root node, 434 obs at root, loss = 195 (out of 434, 195 predicted wrongly), 195/404
                   #lctdir = liberal, 205 obs, loss = 82 (predict 82 ones when y-var actually 0. Say reverse, when nv reverse. 205 - 82 predicted zeroes)   ***revise this
                   #loss is whatever's in the non-majority
 
table(train$rev)   #195 predicted wrongly. Haven't split data, so majority have 1 as output

prp(cart2, type = 1)
prp(cart2, type = 4)
prp(cart2, type = 4, extra = 4)       #at this node, what fraction is 0 or 1.   Same var can appear multiple times. don't want too few observations at leaf node to prevent overfitting. 


