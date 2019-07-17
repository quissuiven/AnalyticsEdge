
#In CART, splits are made at ea step to make buckets as homogeneous or pure as possible
#don't want too few observations at leaf node to prevent overfitting. Partition until no of obs in leaf node = some min number for all leaf nodes

#Measuring error for classification:
#1) GINI index = p(1-p) + (1-p)p = 2p(1-p)             *where p is proportion of obs that has 0, 1-p is proportion of obs that has 1
#             Want to min Loss function. Would like P to be all 0, or all 1
#2) Entropy = -plogp -(1-p)log(1-p)
#             Similarly, want to min Loss function, would like P to be all 0, or all 1
#3) Misclassification = 1 - max(p, 1-p)

#Pure: more of one class in same node
#Lower error, more pure
#choice of loss criterion v impt, since loss may be the same

#Impurity example
#Impurity can be calculated using any of above 3 methods
#Reduction in Impurity = Impurity (A) - 600/800 * (1/3) + 200/800 (0) = 1/4           #1/3 since 200/600   (minority in left node/total obs in node)
           #more than 0 is better, since reduces impurity

#min Loss(T) + alpha|T|      *where T is no of nodes, alpha|T| to take into acc model complexity
                             #if alpha = 0, build full tree. If alpha = high, build small tree

#typically build full tree first, then start cutting off branches of the tree. Figure out error using validation set (cross-validation)

predictcart1 <- predict(cart2, newdata = test, type="class")
predictcart1

table(predictcart1, test$rev)
(54+69)/(54+69+33+28)    #test accuracy = 0.66   (similar for both logistic and CART, 20% improvement from baseline acc of 0.55)

table(train$rev)
table(test$rev)
(102/(102+82))        #Baseline accuracy = 0.55 

library(ROCR)
predictcart1prob <- predict(cart2, newdata = test)       #probabilities for CART
predictcart1prob

#getting AUC for logistic probabilities
predictroclog <- prediction(p1, test$rev)
performance(predictroclog, measure = "auc")     #AUC = 0.74

#getting AUC for CART probabilities
predictroccart <- prediction(predictcart1prob[,2], test$rev)
performance(predictroccart, measure = "auc")              #AUC = 0.71 < 0.74, therefore logistic does better than cart here

library(rpart)
printcp(cart2)               #cp = alpha. When alpha is large, no splits, all obs in 1 node. rel error like training error (as build tree, error decreases)
                             #xerror is cross validation error, by 10 fold. May not decrease, since out of sample in test set
                             #xstd, standard deviation of cross validation error
                             
#try to get same result
set.seed(1)
cart2 <- rpart(as.factor(rev)~ petit + respon + circuit + unconst + lctdir + issue, data = train)
printcp(cart2) 

cart3 <- prune(cart2, cp = 0.036)       #any value in between CPs, will get same tree. Just want to find out get same tree

predictcart2 <- predict(cart3, newdata= test, type = "class")
table(predictcart2, test$rev)

(63 + 68)/ (63 + 68 + 34 + 19)     #predictive accuracy is 0.71 > 0.66. Shallow tree has better prediction 


#Ensemble methods - Random Forests
#One model may not be the best, can use multiple models (build a few trees, let ea tree to make prediction, then take overall prediction)
#Do something to get different trees
#Key: Randomization
#Step 1: Bootstrap (Bagging): creates data with replacement of original dataset, generate new types of data diff but similar to original
#Step 2: Choose a random subset of potential predictors at each node, later may repeat if randomly pick same vars later on

library(randomForest)
set.seed(100)
forest1<- randomForest(as.factor(rev)~ petit + respon + circuit + unconst + lctdir + issue, data = train, ntree = 200)  #build 200 trees
predictforest <- predict(forest1, newdata=test, type="class")   #output is class
table(predictforest,test$rev)          

(44 + 86)/(44+16+38+86)     #prediction accuracy is 0.70, sophisticated model doesn't help much
