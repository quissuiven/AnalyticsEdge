
# AE PROJECT 2018

###### DATA PREPROCESSING ######

# reading data
train <- read.csv("train.csv")
str(train)           #14250 obs of 113 vars (excluding Choice). Ea customer performs 19 choice tasks.
test <- read.csv("test.csv")
str(test)            #7315 obs of 113 vars (excluding Choice)

library(plyr)
count(unique(test$Case))

# adding Choice column
train$Choice[train$Ch1 == 1] <- "Ch1"
train$Choice[train$Ch2 == 1] <- "Ch2"
train$Choice[train$Ch3 == 1] <- "Ch3"
train$Choice[train$Ch4 == 1] <- "Ch4"
train$Choice<-factor(train$Choice)
#table(train$Choice)      #3226 Ch1, 3637 Ch2, 3163 Ch3, 4224 Ch4
#str(train,list.len=ncol(train)) # dont truncate - list all variables 

# insert dummy data to avoid out of bound error
#test$Ch1=0
#test$Ch2=0
#test$Ch3=0
#test$Ch4=0
test$Choice<-"Ch1"  # seems like only this variable requires a dummy data...
test$Choice<-factor(test$Choice)

View(train)

carData <- matrix(nrow = length(unique(train$No)),ncol = 4)


for (i in 1:nrow(train)){                       #Fill matrix with values of col Ch1
  carData[train$No[i],1]  <- train$Ch1[i]
}

for (i in 1:nrow(train)){                      
  carData[train$No[i],2]  <- train$Ch2[i]
}

for (i in 1:nrow(train)){                      
  carData[train$No[i],3]  <- train$Ch3[i]
}

for (i in 1:nrow(train)){                      
  carData[train$No[i],4]  <- train$Ch4[i]
}

hist(as.vector(carData))

set.seed(1)
carspl1 <- sample(1:nrow(carData),0.98*nrow(carData))                
str(carspl1)                 #13965 obs

set.seed(2)
carspl2 <- sample(1:ncol(carData),0.8*ncol(carData))         #3 obs    (since total only 4 cols)
carspl1c <- setdiff(1:nrow(carData),carspl1)                 #285 obs             
carspl2c <- setdiff(1:ncol(carData),carspl2)                 #1 ob
str(carspl2c)               #1711 obs

#baseline model

carBase1 <-matrix(nrow =length(carspl1c),ncol = length(carspl2c))
carBase2 <-matrix(nrow =length(carspl1c),ncol = length(carspl2c))

for (i in 1:length(carspl1c)){
    carBase1[i,] <- mean(carData[carspl1,carspl2c], na.rm= TRUE)            #can't use colMeans since only 1 column
}

carBase2[,1] <- rowMeans(carData[carspl1c,carspl2],na.rm= TRUE)

RMSEcarBase1 <- sqrt(mean((carBase1 - carData[carspl1c,carspl2c])^2,na.rm = TRUE))   #0.4356  (for prediction only on Ch4)
RMSEcarBase2 <- sqrt(mean((carBase2 - carData[carspl1c,carspl2c])^2,na.rm = TRUE))   #0.5793725


#creating user predictions
carUser <-matrix(nrow =length(carspl1c),ncol = length(carspl2c))
carCor <-matrix(nrow =length(carspl1),ncol = 1)                      #keep track of correlation among users
carOrder <- matrix(nrow = length(carspl1c),ncol=length(carspl1))        #keep track of which users similar, which are not

for(i in 1:length(carspl1c)){
  for (j in 1:length(carspl1)){
    carCor[j] <- cor(carData[carspl1c[i],carspl2],carData[carspl1[j],carspl2],use="pairwise.complete.obs")     #correlate data only on the left side of matrix, should have 691 correlations
  }
  v <- order(carCor, decreasing = TRUE, na.last = NA)                #order correlations from high to low
  carOrder[i,] <- c(v, rep(NA,times = length(carspl1)-length(v)))
}

RMSEcarUser <- sqrt(mean((carData[carspl1c,carspl2c]-carUser)^2,na.rm=TRUE))   



###### DATA ANALYSIS ######

table(train$ageind,train$Ch1)
table(train$ageind,train$Ch2)
table(train$ageind,train$Ch3)
table(train$ageind,train$Ch4)

table(train$genderind,train$Ch1)
table(train$genderind,train$Ch2)
table(train$genderind,train$Ch3)
table(train$genderind,train$Ch4)
#plot(subset(train,train$Ch4==1)$ageind,subset(train,train$Ch4==1)$Ch4)


###### MODEL APPLICATIONS ######

# 1.) mlogit model with all parameters
library(mlogit)
which(colnames(train)=="CC1")
which(colnames(train)=="Price4")
train_logit <- mlogit.data(train,shape ="wide",choice ="Choice", varying = c(4:83), sep = "", alt.levels = c("Ch1","Ch2","Ch3","Ch4"),id.var="Case")
model1 <- mlogit(Choice~CC+GN+NS+BU+FA+LD+BZ+FC+FP+RP+PP+KA+SC+TS+NV+MA+LB+AF+HU+Price-1, data = train_logit1)
summary(model1)
# log-likelihood = -17517

test_logit<-mlogit.data(test,shape ="wide",choice ="Choice", varying = c(4:83), sep = "", alt.levels = c("Ch1","Ch2","Ch3","Ch4"),id.var="Case")
predict1<-predict(model1,newdata=test_logit)
# no validation accuracy due to no validation set
# Achieved 1.21719 on test RMSE in kaggle

######################################################################################

# 2.) mlogit model with all parameters + cross validation

# first, test whether idea of cross validation would work or not...
totalusers<-max(train$Case)
# split data into train and validation set
bottom<-150
val<-tail(train,n=bottom*19)
#str(val)
#table(val$Case)
train_only<-head(train,n=(totalusers-bottom)*19)
#str(train_only)

train_logit<-mlogit.data(train_only,shape ="wide",choice ="Choice", varying = c(4:83), sep = "", alt.levels = c("Ch1","Ch2","Ch3","Ch4"),id.var="Case")
val_logit<-mlogit.data(val,shape ="wide",choice ="Choice", varying = c(4:83), sep = "", alt.levels = c("Ch1","Ch2","Ch3","Ch4"),id.var="Case")
model<-mlogit(Choice~CC+GN+NS+BU+FA+LD+BZ+FC+FP+RP+PP+KA+SC+TS+NV+MA+LB+AF+HU+Price-1, data = train_logit)
summary(model)
predicted_choice<-predict(model,newdata=val_logit)
str(predicted_choice)
actual_choice<-subset(val,select=c(Ch1,Ch2,Ch3,Ch4))
str(actual_choice)

#actual_choice[1,4]
#log(predicted_choice[1,4])

# calculation of RMSE
totallogloss<-0
for (i in 1:dim(actual_choice)[1]){
  for (j in 1:dim(actual_choice)[2]){
    if (actual_choice[i,j]==1){
      totallogloss<-totallogloss+log(predicted_choice[i,j])     
    }
  }
}
logloss<-totallogloss/(-dim(actual_choice)[1])
logloss
# choosing the bottom 50 as validation set produces 1.195457 on validation set
# choosing the bottom 75 as validation set produces 1.20822 on validation set
# choosing the bottom 150 as validation set produces 1.189386 on validation set
# it kinda works..seems like we can find the best model

# lets automate this k-fold cross validation

#install.packages("caret")
library(caret)
?createFolds
#train_index<-1:dim(train)[1]
#folds<-createFolds(train_index,k=10,list=TRUE,returnTrain=TRUE)
#length(folds[[2]])
#length(folds[2])
#9/10*dim(train)[1]
#1/10*dim(train)[1]
#train_only<-train[folds[[2]],]
#dim(train_only)
#val<-train[-folds[[2]],]
#dim(val)

best_rmse<-1000000
best_model<-NA
train_index<-1:dim(train)[1]
# generates 100 itertions for k-folds
for (i in 1:100){
  print(paste("### Iteration: ",as.character(i)," ###"))
  set.seed(i)
  folds<-createFolds(train_index,k=10,list=TRUE,returnTrain=TRUE)
  local_rmse<-0
  for (j in 1:10){
    train_only<-train[folds[[j]],]
    val<-train[-folds[[j]],]
    train_logit<-mlogit.data(train_only,shape ="wide",choice ="Choice", varying = c(4:83), sep = "", alt.levels = c("Ch1","Ch2","Ch3","Ch4"),id.var="Case")
    val_logit<-mlogit.data(val,shape ="wide",choice ="Choice", varying = c(4:83), sep = "", alt.levels = c("Ch1","Ch2","Ch3","Ch4"),id.var="Case")
    model<-mlogit(Choice~CC+GN+NS+BU+FA+LD+BZ+FC+FP+RP+PP+KA+SC+TS+NV+MA+LB+AF+HU+Price-1, data = train_logit)
    #print(summary(model))
    predicted_choice<-predict(model,newdata=val_logit)
    actual_choice<-subset(val,select=c(Ch1,Ch2,Ch3,Ch4))
    rmse<-calculateRMSE(predicted_choice=predicted_choice,actual_choice=actual_choice)
    print(paste("Current RMSE: ",as.character(rmse)))
    local_rmse<-local_rmse+rmse
    
    
    if(rmse<best_rmse){
      best_rmse<-rmse
      best_model<-model
    }
  }
  print(paste("Average RMSE: ",as.character(local_rmse/(i*10))))
  print(paste("Best RMSE: ",as.character(best_rmse)))
  
}

summary(best_model)
best_rmse

######################################################################################

# 3.) mlogit model with demographic parameters

best_rmse<-1000000
second_best_rmse<-1000000
best_model<-NA
second_best_rmse<NA
train_index<-1:dim(train)[1]
local_rmse<-0
# generates 100 itertions for k-folds
for (i in 1:100){
  print(paste("### Iteration: ",as.character(i)," ###"))
  set.seed(i)
  folds<-createFolds(train_index,k=10,list=TRUE,returnTrain=TRUE)
  
  for (j in 1:10){
    train_only<-train[folds[[j]],]
    val<-train[-folds[[j]],]
    train_logit<-mlogit.data(train_only,shape ="wide",choice ="Choice", varying = c(4:83), sep = "", alt.levels = c("Ch1","Ch2","Ch3","Ch4"),id.var="Case")
    val_logit<-mlogit.data(val,shape ="wide",choice ="Choice", varying = c(4:83), sep = "", alt.levels = c("Ch1","Ch2","Ch3","Ch4"),id.var="Case")
    model<-mlogit(Choice~CC+GN+NS+BU+FA+LD+BZ+FC+FP+RP+PP+KA+SC+TS+NV+MA+LB+AF+HU+Price|segmentind+yearind+milesind+nightind+pparkind+genderind+ageind+educind+regionind+Urbind+incomeind-1, data = train_logit)
    predicted_choice<-predict(model,newdata=val_logit)
    actual_choice<-subset(val,select=c(Ch1,Ch2,Ch3,Ch4))
    rmse<-calculateRMSE(predicted_choice=predicted_choice,actual_choice=actual_choice)
    print(paste("Current RMSE: ",as.character(rmse)))
    local_rmse<-local_rmse+rmse
    
    #print(best_rmse)
    #print(second_best_rmse)
    
    if(rmse<best_rmse){
      second_best_rmse<-best_rmse
      second_best_model<-best_model
      best_rmse<-rmse
      best_model<-model
    }
    else if(rmse<second_best_rmse){
      second_best_rmse<-rmse
      second_best_model<-model
    }
  }
  print(paste("Average RMSE: ",as.character(local_rmse/(i*10))))
  print(paste("Best RMSE: ",as.character(best_rmse)))
  print(paste("Second Best RMSE: ",as.character(second_best_rmse)))
}

summary(best_model)
best_rmse
summary(second_best_model)
second_best_rmse

# for some reason, the demographics information seems to be a good predictor of Ch4
# maybe we can use demographics for predicting Ch4 only?

subset(train_logit,train_logit$Task==17)

######################################################################################

# 4.) mlogit model with parameters with significance level of ***
#M2 <- mlogit(Choice~CC+GN+NS+BU+LD+KA+SC+TS+MA+LB+HU+Price-1,data=S1)






###### MISC FUNCTIONS ######

# calculate RMSE score
calculateRMSE<-function(predicted_choice, actual_choice){
  totallogloss<-0
  for (i in 1:dim(actual_choice)[1]){
    for (j in 1:dim(actual_choice)[2]){
      if (actual_choice[i,j]==1){
        totallogloss<-totallogloss+log(predicted_choice[i,j])     
      }
    }
  }
  logloss<-totallogloss/(-dim(actual_choice)[1])
  return(logloss)
}

# to obtain predicted choice given probabilities in a row
PredictedChoice<-apply(TestPredict3,1,which.max)

# creating a matrix with one-prediction for each row
dmatrix<-matrix(nrow = length(PredictedChoice), ncol = 4)
for(i in 1:length(PredictedChoice)){
  for(j in 1:4){
    if (j==PredictedChoice[i]){
      dmatrix[i,j]<-1
    }
    else{
      dmatrix[i,j]<-0
    }
  }
}

# writing to csv file
write.csv(dmatrix,"wow.csv")
write.csv(TestPredict3,"wow2.csv")

