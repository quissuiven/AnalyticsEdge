
#Text Analytics, Clustering in R

#Question 1
#part a
emails <- read.csv("emails.csv", stringsAsFactors = FALSE)       #string read in as character vector. Needed for text analytics
str(emails)                 #5728 emails
table(emails$spam)          #4360 non-spam, 1368 spam
summary(emails)             #1st col is text, 2nd col is spam. 

#part b
email[1]            #start with 'Subject:'
strwrap(emails[1,1])              #Split character string into paras, paras formatted. Choose 1st row (1st email), 1st col(text, not spam)
strwrap(emails[1000,1])

#part c
#Yes. No of times it appears might be different in spam vs nonspam emails.
#Eg. long email may have 'Subject:' occur more often and therefore indicate spam

#part d
nchar(emails[1,1])          #first email has 1484 chars
nchar(emails$text[1])       #same as above
max(nchar(emails$text))     #Max no of characters in one email is 43952

#part e
which.min(nchar(emails$text))   #row 1992 has fewest no of characters in one email
min(nchar(emails$text))         #with only 13 characters
emails$text[1992]      #13 chars (Subject: fyi)
emails[1992,1]         #same as above

#part f
library(tm)    #tm = textmining
corpusemails <- Corpus(VectorSource(emails$text))    #create structure for preprocessing
corpusemails[[1]]    #get details of first doc
as.character(corpusemails[[1]] )      #get contents of first doc    

corpusemails <- tm_map(corpusemails, tolower)    #change all uppercase to lowercase
as.character(corpusemails[[1]] ) 

corpusemails <- tm_map(corpusemails, removePunctuation)
as.character(corpusemails[[1]] ) 

corpusemails <- tm_map(corpusemails, removeWords, stopwords("English"))    #drop all stopwords. But sometimes, certain stopwords are useful eg. cannot. Need to make choices
as.character(corpusemails[[1]] ) 

library(SnowballC) 
corpusemails <- tm_map(corpusemails, stemDocument)          #stem doc. catty -> cat
as.character(corpusemails[[1]] ) 

dtm <- DocumentTermMatrix(corpusemails)       #create matrix of docs by terms/words. Matrix shows frequency of terms for ea doc
dtm                                           #sparse entries are entries which have 0
#dtm has 5728 docs, 28687 terms

#part g
spdtm <- removeSparseTerms(dtm, 0.95)   #remove terms which have more than 95% sparsity, appear <5% of docs
spdtm         
#spdtm has 330 terms

#part h
emailsSparse <- as.data.frame(as.matrix(spdtm)) 
colnames(emailsSparse) <- make.names(colnames(emailsSparse))   #make var names valid 
colSums(emailsSparse)                   #returns no of times a word stem/ variable appears
which.max(colSums(emailsSparse))        #emron, 324th term
max(colSums(emailsSparse))              #occurs 13388 times

#part i
emailsSparse$spam <- emails$spam
hamemails <- subset(emailsSparse,emailsSparse$spam==0)      #create a subset of emailsSparse that only contains ham emails. Seems that emailsSparse[emailsSpare$spam == 0] doesn't work on data frames
colSums(hamemails)    #shows no of times word stems occur in ham email subset
sort(colSums(hamemails))       #sort by no of times.    hou to emron occur more than 5000 times

library(plyr)
count(colSums(hamemails)>=5000) #shows that 6 word stems >= 5000 times

#part j
spamemails <- subset(emailsSparse,emailsSparse$spam==1) 
sort(colSums(spamemails))      #compani, spam, will, subject occur >= 1000 times

#part k
#Frequencies of these most common words are likely to help differentiate btw spam and ham words
#If emron occur frequently -> ham. If subject occur frequently -> spam

#part l
#since specific to Vincent's inbox, models we build are personalized, need to be further tested as spam filter

#part m 
emailsSparse$spam <- as.factor(emailsSparse$spam)
set.seed(123)
library(caTools)
split <- sample.split(emailsSparse$spam,SplitRatio = 0.7)
spamtrain <- subset(emailsSparse, split == TRUE)
spamtest <- subset(emailsSparse, split == FALSE)
table(spamtrain$spam)   
table(spamtest$spam)          #baseline accuracy = 958/ (3052 + 958) =  0.2389027

#Building logistic regression model
spamLog <- glm(spam~., data= spamtrain, family = binomial)
summary(spamLog)       

predictLog <- predict(spamLog, newdata= spamtrain, type ="response")     #find predicted spam probabilities for training set, not test set
table(predictLog)                #predicts a probability of whether an email is spam
table(predictLog < 0.00001)      #3046
table(predictLog > 0.99999)      # 954
table(predictLog >= 0.00001 & predictLog <= 0.99999)     #10

#building CART model
library(rpart)
library(rpart.plot)
set.seed(123)
spamCart <- rpart(spam~., data=spamtrain)  
prp(spamCart)                    

predictCart <- predict(spamCart, newdata= spamtrain)
predictCart <- predictCart[,2]

#building random forest model
library(randomForest)
set.seed(123)
spamForest <- randomForest(spam~., data = spamtrain)       #OOB: Out of Bag/ Subset of data. Error in training set        
spamForest                                           

predictForest <- predict(spamForest, newdata= spamtrain, type="prob")    #"prob" since predicting probability. 1st col not spam, second col spam
predictForest <- predictForest[,2]    #keep 2nd col, probabilities of whether spam

#part n
#none are sig

#part o
prp(spamCart)       #vine and emron appear at the top of the CART model. Hou and kaminski do not appear

#part p, q, r, s, t (Find accuracies/auc for predictions on training set)
table(predictLog >= 0.5 , spamtrain$spam)             #accuracy = (954 + 3056) / (954 + 3056 + 4 + 0) = 0.9990035
library(ROCR)
predROCR <- prediction(predictLog, spamtrain$spam)                         #predictLog, not spamLog. Use predictions as basis
auc <- performance(predROCR, measure = "auc")         #auc = 0.9999959 

table(predictCart >= 0.5 , spamtrain$spam)            #accuracy = (2885 + 894)/ (2885 + 894 + 64 + 167) = 0.942394
predROCR2 <- prediction(predictCart, spamtrain$spam)
auc2 <- performance(predROCR2, measure = "auc")       #auc = 0.9696044

table(predictForest >= 0.5 , spamtrain$spam)          #accuracy = (3046 + 958)/(3046 + 958 + 0 + 6) = 0.9985037
predROCR3 <- prediction(predictForest, spamtrain$spam)
auc3 <- performance(predROCR3, measure = "auc")       #auc = 0.9999959

#part u
#Logistic Regression has best training set accuracy and AUC

#part v, w (Find accuracies/auc for predictions on testing set)
predictLogtest <- predict(spamLog, newdata= spamtest, type ="response")
table(predictLogtest >= 0.5 , spamtest$spam)             #accuracy = (1257 + 376)/(1257 + 376 + 34 + 51) = 0.9505239
predROCRtest <- prediction(predictLogtest, spamtest$spam)                         
auctest <- performance(predROCRtest, measure = "auc")    #auc = 0.9627517

predictCarttest <- predict(spamCart, newdata= spamtest)
predictCarttest <- predictCarttest[,2]
table(predictCarttest >= 0.5 , spamtest$spam)            #accuracy = (1228 + 386)/ (1228 + 386 + 24 + 80) = 0.9394645
predROCRtest2 <- prediction(predictCarttest, spamtest$spam)
auctest2 <- performance(predROCRtest2, measure = "auc")  #auc = 0.963176

predictForesttest <- predict(spamForest, newdata= spamtest, type="prob")   
predictForesttest <- predictForesttest[,2]  
table(predictForesttest >= 0.5 , spamtest$spam)          #accuracy = (1290 + 385)/(1290 + 385 + 25 + 18) = 0.9749709
predROCRtest3 <- prediction(predictForesttest, spamtest$spam)
auctest3 <- performance(predROCRtest3, measure = "auc")  #auc = 0.997768

#part x
#Random Forest has best testing set accuracy and AUC

#part y
#Logistic Regression has greatest degree of overfitting, since it has almost perfect fit on training set but not as good on testing set.
#CART and RF have similar accuracies on both training and test sets


#Question 2
#Cluster-then-predict: cluster obs, then build cluster-specific prediction models
#1) Cluster stocks that have similar returns over time
#2) Logistic regression to predict whether stocks will have positive future returns

#part a
stocks <- read.csv("StocksCluster.csv")
str(stocks)                 #11580 obs, each obs is monthly returns of a coy in a year
summary(stocks)             #11 vars for return from Jan to Nov, 1 var for whether positive return in Dec
                            #For 11 vars, value is % change in stock value.

#part b
table(stocks$PositiveDec)   #6234/ (5256 + 6324) = 0.538342 of obs have positive returns in December

#part c
cor(stocks$ReturnJan,stocks$ReturnFeb,use="pairwise.complete.obs")   #pairwise correlation btw ReturnJan and ReturnFeb = 0.06677458
cor(stocks[,1:11])         #shows table of correlations between any 2 vars. Select all rows, cols 1 to 11.
sort(cor(stocks[,1:11]))   #max correlation = 0.1916727856

#part d
colMeans(stocks[,1:11])        #shows mean return for each month
sort(colMeans(stocks[,1:11]))  #Smallest is Sep. Largest is Apr

#part e
set.seed(144)
spl <- sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain <- subset(stocks, spl == TRUE)
stocksTest <- subset(stocks, spl == FALSE)

StocksModel <- glm(PositiveDec~., data = stocksTrain, family = binomial)
predictStocks <- predict(StocksModel, newdata= stocksTrain, type ="response") 
table(predictStocks >= 0.5 , stocksTrain$PositiveDec)        #Accuracy = (3640 + 990)/ (3640 + 990 + 787 + 2679) = 0.5718874

#part f
predictStockstest <- predict(StocksModel, newdata= stocksTest, type ="response") 
table(predictStockstest >= 0.5 , stocksTest$PositiveDec)     #Accuracy = (417 + 1553)/ (417 + 1553 + 344 + 1160) = 0.5670697

#part g
table(stocksTrain$PositiveDec)    #Most common outcome is 1
table(stocksTest$PositiveDec)     #1897/ (1577 + 1897) = 0.5460564

#part h (Clustering)
limitedTrain <- stocksTrain
limitedTrain$PositiveDec <- NULL
limitedTest <- stocksTest
limitedTest$PositiveDec <- NULL
#Removing dependent vars, since needing to know dependent var defeats purpose of clustering

#part i
#In some cases with training and test set, might wan to normalize by mean and sd of vars in training set
library(caret)
preproc <- preProcess(limitedTrain)  #Normalizes vars by subtracting mean and dividing by sd
normTrain <- predict(preproc,limitedTrain)
normTest <- predict(preproc,limitedTest)
colMeans(normTrain)        #mean of ReturnJan in Training set as normalized close to 0
colMeans(normTest)         #mean of ReturnJan in Testing set as normalized is -0.0004185886

#part j
#mean of ReturnJan in Training set closer to 0 due to diff distributions in Training set and Testing set

#part k (Implement K-means clustering)
set.seed(144)
km <- kmeans(normTrain,centers=3)
km$centers                   #cluster means           
table(km$cluster)            #Cluster 2 has largest no of obs 4696

#part l (Preparing Training set cluster and Testing set cluster for predictions)
library(flexclust)
km.kcca <- as.kcca(km, normTrain)
clusterTrain <- predict(km.kcca)
clusterTest <- predict(km.kcca,newdata = normTest)
table(clusterTest)           #Cluster 2 has 2080 testing set obs

#part m
stocksTrain1 <- subset(stocksTrain, clusterTrain == 1)    #subset stocksTrain based on no of obs in cluster 1, 3157 obs
stocksTrain2 <- subset(stocksTrain, clusterTrain == 2)    #4696 obs
stocksTrain3 <- subset(stocksTrain, clusterTrain == 3) 
  
stocksTest1 <- subset(stocksTest,clusterTest == 1)
stocksTest2 <- subset(stocksTest,clusterTest == 2)
stocksTest3 <- subset(stocksTest,clusterTest == 3) 

colMeans(stocksTrain1)     #PositiveDec avg is 0.60247
colMeans(stocksTrain2)     #0.51405
colMeans(stocksTrain3)     #0.43873

#stocksTrain1 has highest avg value of dependent var

#part n  (building Logistic regression models for each training set cluster)
StocksModel1 <- glm(PositiveDec~., data = stocksTrain1, family = binomial)
summary(StocksModel1)

StocksModel2 <- glm(PositiveDec~., data = stocksTrain2, family = binomial)
summary(StocksModel2)

StocksModel3 <- glm(PositiveDec~., data = stocksTrain3, family = binomial)
summary(StocksModel3)

#vars that have at least one +ve and one -ve coeff: returnJan, returnFeb, returnMar, returnJune, returnAug, returnOct

#part o (doing predictions using each model on corresponding testing set cluster)
predictTest1 <- predict(StocksModel1, newdata= stocksTest1, type ="response") 
table(predictTest1  >= 0.5 , stocksTest1$PositiveDec)     #Accuracy = (30 + 774)/(30 + 774 + 23 + 471) = 0.61941

predictTest2 <- predict(StocksModel2, newdata= stocksTest2, type ="response") 
table(predictTest2  >= 0.5 , stocksTest2$PositiveDec)     #Accuracy = (388 + 757)/(388 + 757 + 309 + 626) = 0.55048

predictTest3 <- predict(StocksModel3, newdata= stocksTest3, type ="response")   
table(predictTest3  >= 0.5 , stocksTest3$PositiveDec)     #Accuracy = (49 + 13)/(49 + 13 + 21 + 13) = 0.64583

#part p   (For overall test-set accuracy, combine test-set predictions into a single vector, all true outcomes into a single vector)
AllPredictions <- c(predictTest1, predictTest2, predictTest3)
AllOutcomes <- c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
table(AllPredictions  >= 0.5 , AllOutcomes)               #Overall Accuracy = (467 + 1544)/ (467 + 1544 + 353 + 1110) = 0.5788716  > 0.5670697 (acc w/o clustering)



#Question 3
#part a
citi <- read.csv("citibike.csv")
unique(citi$startstation)
nlevels(citi$startstation)         #329 levels
unique(citi$endstation)
nlevels(citi$endstation)           #329 levels
#329 bike stations

#part b
tapply(citi$tripduration,citi$day,mean, na.rm=TRUE)  #for each day's trip duration, find avg.  Sat has the highest avg. 

#part c
table(citi$starttime)    #no of trips at ea start time
which.max(table(citi$starttime))    #18th hour, 6pm with max no of bikes
which.min(table(citi$starttime))    #4th hour, 4am with min no of bikes

#part d
table(citi$gender)   #156912/ (156912 + 510816)= 0.2349939 

#part e
citi$Mon[citi$day == "Mon"] <- 1
citi$Mon <- as.integer(citi$day == "Mon")          #either works

#part f
summary(citi)        #tripduration would dominate since the no in seconds is v high

#part g
citi$Tue <- as.integer(citi$day == "Tue") 
citi$Wed <- as.integer(citi$day == "Wed") 
citi$Thu <- as.integer(citi$day == "Thu") 
citi$Fri <- as.integer(citi$day == "Fri") 
citi$Sat <- as.integer(citi$day == "Sat") 
citi$Sun <- as.integer(citi$day == "Sun") 
citi1 <- citi
citi1$tripduration <- scale(citi1$tripduration)
citi1$gender <- scale(citi1$gender)
citi1$age <- scale(citi1$age)
citi1$starttime <- scale(citi1$starttime)
citi1$Mon <- scale(citi1$Mon)
citi1$Tue <- scale(citi1$Tue)
citi1$Wed <- scale(citi1$Wed)
citi1$Thu <- scale(citi1$Thu)
citi1$Fri <- scale(citi1$Fri)
citi1$Sat <- scale(citi1$Sat)
citi1$Sun <- scale(citi1$Sun)

max(citi1$tripduration)         #402.9514

#part h
#data has 667738 obs. It will be hard for hierarchical clustering to handle computing the distances btw all these observations.
#Since quadratic computational power in no of obs

#part i
set.seed(100)
cluster1 <- kmeans(citi1[,c("tripduration", "gender", "age", "starttime", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")],centers=10)
summary(cluster1)
cluster1$size       #min is 18148 trips, max is 107185 trips

#part j (cluster centres are the means)
cluster1$centers     #cluster 5 seems to have good representation of older drivers driving on Sat               

#part k
#cluster 10 describes longer trips taken primarily by females on Tue or Wed

#part l
#different results  (without calling set.seed)

#part m
#identical results (after calling set.seed(100) again)

#part n
View(citi)
citi$weekday[citi$day == "Mon" | citi$day == "Tue" | citi$day == "Wed" | citi$day == "Thu" | citi$day == "Fri"] <- 1
citi$weekday[citi$day == "Sat" | citi$day == "Sun"] <- 0
citi2<- citi
citi2$weekday <- scale(citi2$weekday)
set.seed(100)
cluster2 <- kmeans(citi2[,c("tripduration", "gender", "age", "starttime", "weekday")],centers=10)
cluster2$centers 
#Cluster 1 best fits description of longer trips by older females on weekdays

#part o
#Cluster 4
# 