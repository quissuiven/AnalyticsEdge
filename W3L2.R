

#if P(Field = 1) > t     =>   Predict Field = 1(Failure)        No launch
#                <= t    =>   Predict Field = 0(No Failure)     Launch

#construct confusion matrix
# accuracy = TN + TP/ TN + TP + FN + FP
# false positive rate = FP/ FP + TN    As low as possible.   
# false positive = predict positive when shld not
# false negative = predict negative when shld not
# Sensitivity = true positive rate = TP/ TP + FN     As high as possible, measures rate positives classify correctly
# Specificity = true negative rate = TN/ TN + FP     

install.packages("ROCR")
library(ROCR)
Pred <- predict(model4,newdata=orings,type="response")
table(Pred>0.5,orings$Field[1:138])          #0.5 is the threshold
table(Pred[1:138]>0.5,orings$Field[1:138])                        #FPR, TPR function of t. Changes as t changes
table(Pred[1:138]>0.25,orings$Field[1:138])

#ROC curve(Receiver operating characteristic)
#TPR against FPR
# all observations are plotted on the straight diagonal line
#Perfect model, Area under curve =1
#Random guessing, auc = 0.5
#want to curve upwards, since want TPR to be greater/ auc greater
#for 1 curve, 1 point is not better than another point (since TPR smaller, FPR also smaller)
ROCRpred <- prediction(Pred[1:138],orings$Field[1:138])
ROCRpred

ROCRperformance <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")
ROCRperformance
plot(ROCRperformance)

performance(ROCRpred, measure = "auc")        #auc = 0.725    auc greater then better, since means TPR greater



#framingham study
framing <- read.csv("framingham.csv")
framing1 <- subset(framing,PREVCHD == 0 & PERIOD == 1)
framing1$TENCHD <- as.integer(framing1$TIMECHD/365 <= 10)
max(framing1$TENCHD)
max(framing1$TIMECHD)
which(colnames(framing1)== "TIMECHD")
which(colnames(framing1)== "TENCHD")
framing1 <- framing1[,c(1:21, 40)]
head(framing1)

library(caTools)

split <- sample.split(framing1$TENCHD,SplitRatio = 0.65)    #randomly picks observations to key
head(split)
set.seed(1)
split <- sample.split(framing1$TENCHD,SplitRatio = 0.65)
head(split)

training <- subset(framing1, split == TRUE)
test <- subset(framing1, split == FALSE)
str(training)
str(test)

table(training$TENCHD)
table(test$TENCHD)
419/(419+2337)
225/(225+1259)

m1 <- glm(TENCHD~., data= training, family = binomial)
summary(m1)


m2 <- glm(TENCHD~SEX + AGE + SYSBP + CIGPDAY + GLUCOSE, data = training, family = binomial)
summary(m2)

predict2 <- predict(m2, type="response", newdata = test)
table(predict2 > 0.5, test$TENCHD)                  #test set (out of sample test). Above 0.5, will get heart disease

(1113+21)/ (1113+21+9+187)            #0.8526316 accuracy, when threshold is 0.5

table(training$TENCHD)            #majority from training set no CHD
table(test$TENCHD)
1259/ (1259+225)                       #0.848, 0.848 probability of getting heart disease        #baseline model

table(predict2 > 0.25, test$TENCHD)
(987 + 87)/(987+121+135+87)           #0.8075.  Lower threshold, more prone to make false positives

#building ROCR curve
library(ROCR)
predict <- prediction(predict2, test$TENCHD)
perf <- performance(predict, measure = "tpr", x.measure ="fpr")
plot(perf)                        #better than random guessing
performance(predict, measure ="auc")

#convert to point system to make it easy to use
#base: points = 0, is a choice
#base difference: difference in reference value from base
#beta coefficient: logit unit/ base difference, how much change as x change   ***need to confirm again
#if male, worse off than female
#once get coefficients, after determine points for Age, use same scale for other vars.
#choose 0.59, smallest no, divide by (0.059 * 5) to make 2, an integer
#