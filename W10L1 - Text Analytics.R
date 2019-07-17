
#Sentiment analysis/ Text analytics
#Data not always structured. Data is textual, so need to preprocess to use it.
#can obtain data from many media: TWitter, Facebook

#Sentiment analysis: given a text, want to see polarity (positive, negative, neutral)
#human can do this, but v time consuming. Want to automate

#Challenges: textual data, with poor spellings (short forms), non-traditional grammar and multilingual
#Ambiguity in sentences: need additional information.
#Possibly using emoticons in tweets to categorize sentences

#Bag of words model:   
#Text = {Bag of words}
#documents by terms(words) matrix. How many times words occur in document. But loses the order, which one occurs first.
#but matrix can end up being v large. Want to reduce the size by preprocessing (removing punctuation, uppercases)

twitter <- read.csv("twitter.csv")
str(twitter)       #entire text is one factor variable

twitter <- read.csv("twitter.csv", stringsAsFactors = FALSE)
str(twitter)

twitter$Negative <- as.factor(twitter$sentiment <= 2)  
table(twitter$Negative)
775/2664

library(tm)    #tm = textmining
corpus <- Corpus(VectorSource(twitter$tweet))    #create structure for preprocessing
corpus[[1]]                        #get details of first doc
as.character(corpus[[1]] )          #get tweet of first doc
as.character(corpus[[2664]] ) 

#preprocessing
#1. to lower case
#2. remove stopwords and other possible words
#3. remove punctuation
#4. stem document  (need SnowballC library)

corpus <- tm_map(corpus, tolower)    #change to lower case
as.character(corpus[[1]] )          #all capital letters change to lower case

stopwords("english")                #want to drop all these in text eg. when, before, after

corpus <- tm_map(corpus, removeWords, stopwords("English"))    #drop all stopwords. But sometimes, certain stopwords are useful eg. cannot. Need to make choices
as.character(corpus[[1]] ) 

#challenge after preprocessing, harder to interpret. But easier for computer to manage.

corpus <- tm_map(corpus, removeWords, c("drive","driver", "driving", "self-driving", "car", "cars"))    #drop words that happen often, that won't help in sentiment 
as.character(corpus[[1]] ) 

corpus <- tm_map(corpus, removePunctuation)
as.character(corpus[[1]] ) 

#stemming: many words come from the same root word eg. cat, catty
#stem all variations to its root word
library(SnowballC)
corpus <- tm_map(corpus, stemDocument)
as.character(corpus[[1]] ) 

freq <- DocumentTermMatrix(corpus)       #create matrix using corpus
freq                                    #sparse entries are entries which have 0. Thus, almost all are 0

freq[1,]            #first doc has 5 non-zero entries
inspect(freq[1,])
findFreqTerms(freq, lowfreq= 50)      #which terms occur at least 50 times

freq[,"day"]        #word "day" occurs in 49 entries, but 50 times
(freq[,"day"]==1)    #48 docs where "day" appears once
(freq[,"day"]==2)    #1 doc where "day" appears twice

freq <- removeSparseTerms(freq, 0.995)  #anything w 99.5% sparsity and more will drop
freq                #matrix less wide, terms from 5618 to 265. More manageable, can then use models like classification, regression

#2 more steps before able to apply models
twittersparse <- as.data.frame(as.matrix(freq))     #convert matrix to dataframe. Use as.matrix since can't directly convert
str(twittersparse)

colnames(twittersparse) <- make.names(colnames(twittersparse))   
twittersparse$Neg <- twitter$Neg               #add output

#Applying models to matrix containing most freq occurring terms, to predict Neg
library(caTools)
set.seed(123)
spl <- sample.split(twittersparse$Neg, SplitRatio = 0.7)
train <- subset(twittersparse,spl == TRUE)
test <- subset(twittersparse,spl == FALSE)
table(train$Neg)   
table(test$Neg)       #baseline accuracy = 567(567 +233) = 0.70875

#LOGISTIC REGRESSION
model1 <- glm(Neg~., data= train, family = binomial)
summary(model1)       #driverless, more negative tweets (coeff is postive). Cool as positive (coeff is negative)

predict1 <- predict(model1, newdata= test, type ="response")
table(predict1>= 0.5, test$Neg)       #Logistic regression accuracy: 479 + 110/ 479 + 110 + 123 + 88 = 0.736

#CLASSIFICATION TREE
library(rpart)
library(rpart.plot)
set.seed(111)
model2 <- rpart(Neg~., data=train)  #vars alrdi factors, therefore will build classification, not regression tree
prp(model2)                    #if job less, trust less, more likely to be negative tweet
                               #Check point in data. Point occurs more in negative tweets than possible, thats why show up 

predict2 <- predict(model2, newdata= test, type = "class")
table(predict2, test$Neg)           #Classification tree accuracy: 560 + 16/ 560 + 217 + 23 = 0.72 (lower, but predicts negative tweets more accurately)

#RANDOM FOREST 
library(randomForest)
set.seed(112)
model3 <- randomForest(Neg~., data = train)       #OOB: Out of Bag/ Subset of data. Error in training set        
model3                                            #

predict3 <- predict(model3, newdata= test, type="class")
table(predict3, test$Neg)       #500 + 91/ 500 + 91 + 142 + 67 = 0.738 (a bit better than logistic)


#NAIVE BAYES CLASSIFIER: another model used for text analytics
#estimate P(doc in class), 2 parameters(mean and sd) then make prediction

library(e1071)          #changing logic: given output, probability of input
model4 <- naiveBayes(Neg~., data=train)
summary(model4)
model4$apriori     #distribution of output. FALSE: tweet positive, TRUE: tweet negative
model4$tables      #classes: FALSE/TRUE. estimating mean and sd.      Eg. worri occurs, find mean and sd to represent likelihood worri occurs

predict4 <- predict(model4, newdata = test, type = "class")        #use threshold of 0.5 to predict majority class

table(predict1 >= 0.5, test$Neg)
table(predict2, test$Neg)
table(predict3, test$Neg)
table(predict4, test$Neg)        #better in negative sentiment tweets (true-true)
