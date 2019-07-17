
#Censored data 
#Problems which large quantities censor the data. If demand for football match exceed 500, don't know how much exact demand
#Q: Only see function Y instead of Y*. How to figure out Beta then?

#Censored regression (Tobit model)
#left censoring at 0: All values < 0 become 0 
#new formula: beta doesn't tell change in function, with one unit change in x. But times P(Bx + episilon >= 0), chances more than 0

#Max likelihood estimation
#Combined obj function: compares Y<0 and Y>= 0. When Y<0, when to max P of Y<0. When Y>= 0, want to max Yi looking at pdf of standard normal var
#Too complex to maximize. Redefine 2 new vars, A = beta/ sigma, B = 1/sigma. To linearize function

x <- seq(-10,10,by=0.001)
plot(x,pnorm(x))
plot(x,log(pnorm(x)))

exm <- read.csv("extramarital.csv")
str(exm)
plot(exm$time_in_affairs)              #time_in_affairs is yi
hist(exm$time_in_affairs,100)          #large no of values with value 0
table(exm$time_in_affairs==0)          #2/3 of data have value 0

#building Tobit model
library(survival)
set.seed(100)
spl <- sample(nrow(exm),0.7*nrow(exm))
train <- exm[spl,]
test <- exm[-spl,]
m1 <- survreg(Surv(time_in_affairs, time_in_affairs>0, type = "left")~.,data= train, dist = "gaussian")     #want to build model on this survival object
summary(m1)                    #log likelihood of -5437
predict1 <- predict(m1, newdata = test)

linearm1 <- lm(time_in_affairs~., data = train)            #not best way to model censored data, due to distinct clumps
summary(linearm1)
predict2 <- predict(linearm1, newdata = test)

table(predict1 <= 0, test$time_in_affairs == 0)           #Tobit model predicts TRUE-TRUE better -> better accuracy for large nos of individuals for whom extramarital affairs is 0
table(predict2 <= 0, test$time_in_affairs == 0)

#Duration Survival Data in Healthcare
#Survival time for patients is at least t but not exactly t (Censored data)
#1. survival function S(t), T = random duration.    To model ppl dying after a certain duration
#2. Hazard rate (rate at which events are completed after duration t) = density fn/ survival fn

#Kaplan- Meier estimation (non-parametric estimation of survival function)

heart <- read.csv("heart.csv")
head(heart)   #people w heart issue waiting for heart transplant

unique(heart$id)

subset(heart, id==1)       #Patient id 1: event = 1, person dies
subset(heart, id==2)
subset(heart, id==4)        #waited 36 days for transplant, died 3 days after transplant
subset(heart, id==25)       #waited 25 days for transplant, until 1800 days still haven't died at end of programme. Therefore survival function, won't go to 0

kaplanmeier <- survfit(Surv(start,stop,event)~1,data=heart)         #start with simplest model, completely non-parametric
plot(kaplanmeier)           #dotted lines: confidence levels. How survival rate changes over time for this group of patients
kaplanmeier
summary(kaplanmeier)
summary(kaplanmeier, censored = TRUE)

subset(heart, stop==1)     #at time 1. 1 person dies. Therefore 102/103. Survival rate = 0.990
subset(heart, id==3)

subset(heart, stop==2)     #at time 2, 3 ppl died. Therefore 99/102. Survival rate = 102/103 * 99/102 = 0.961.  Survival fn drops more rapidly when more ppl die.
subset(heart, stop == 11)     #1 censored obs without death. Patient dropped from time 11
subset(heart, id == 102)

#Cox proportional hazard model
#want to estimate beta

cox <- coxph(Surv(start, stop, event)~age + surgery + transplant, data = heart)   
summary(cox)
plot(survfit(cox))
