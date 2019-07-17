
#Question 1
#Latent semantic indexing: 
#Applies SVD to create low dimensional representation of data to capture semantic similarity of words
#IsiWords.txt: list of 460 unique terms/words that in occurs in set of 9 docs
#IsiMatrix.txt: document by term matrix

#part a

lsiMatrix <- read.table("lsiMatrix.txt")
X <- as.matrix(t(lsiMatrix))
str(X)                    #460 vars (terms), 9 obs (docs)
dim(X)                    #460 by 9
m <- svd(X)
str(m)           #d contains 9 singular values, u contains 460 x 9 left singular values, v contains 9 by 9 right singular values

mreduced <- m$u[,1:2] %*% diag(m$d[1:2]) %*% t(m$v[,1:2])    #Rank 2 approx to matrix, using first 2 singular values

plot(m$v[,1],m$v[,2])             #plot low dimension representation of 9 docs in 2 dimensions, 
                                  #using right singular vectors (first 2 columns) 

text(m$v[,1],m$v[,2],c(1:9), adj =2)   #add text to plot. x coord, y coord, labels to be written, write text adj to point

#docs 7 and 8 closest to ea other


#part b

q <- matrix(0, nrow = 460, ncol = 1)
q[23] <- 1         #make row 23 have value of 1

hatq <- solve(diag(m$d[1:2]))%*%t(m$u[,1:2])%*%q
str(hatq)          #2 dimensional vector

cosine <- matrix(0,9,1)        #for all 9 docs

for (i in 1:9){
  cosine[i] <- sum(hatq * m$v[i,1:2])/sqrt(sum(hatq^2)*sum(m$v[i,1:2]^2))             #From week 10 Recommender systems: alt to Pearson correlation
}

order(cosine, decreasing = TRUE)    # 1 3 2 9 5 8 7 6 4

#top 3 matches: 1, 3, 2



#Question 2
#part a
mroz = read.csv("mroz.csv")
str(mroz)
mroz$exper2 <- (mroz$exper)^2
model1 <- lm(hours~.,data = mroz)
summary(model1)                         #R squared is 0.2656, adjusted R squared is 0.2587
cor(model1$fitted, mroz$hours)^2

                              
#hours = 1330.4824 + (-442.0899)kidslt6 + (-32.7792)kidsge6 + (-30.5116)age + (28.7611)educ + (65.6725)exper + (-3.4466)nwifeinc + (-0.7005)exper^2
#exper2 is significant at 5% level
#Since negative coeff, Experience has decreasing marginal effect on wage

#part b
summary(model1$fitted)         #range from -719.8 to 1614.7
library(plyr)
count(model1$fitted<0)         #39 fitted values below 0,   Censored data
count(mroz$hours==0)        #325 values in dataset have value of 0  (significantly more than that from linear regression model)

#part c
library(survival)
model2 <- survreg(Surv(hours, hours>0, type = "left")~.,data=mroz , dist = "gaussian")      #data left censored at 0, error terms assumed to be gaussian
summary(model2)
#Signs consistent with linear regression model

#part d
p2 <- predict(model2, newdata = mroz)
predict2 <- (p2*pnorm(p2/model2$scale))+(model2$scale*dnorm(p2/model2$scale))
cor(predict2, mroz$hours)^2       #0.2742441, slightly higher than linear regression.   Tobit not designed to max R^2, but max likelihood

#part e
const1 <- model1$coef[1] + model1$coef[2]*mean(mroz$kidslt6) + model1$coef[3]*mean(mroz$kidsge6) + model1$coef[4]*mean(mroz$age) + model1$coef[6]*mean(mroz$exper) + model1$coef[7]*mean(mroz$nwifeinc) + model1$coef[8]*mean(mroz$exper2)      
                                #parts of linear eqn that remain const, only educ varies

const1 + model1$coef[5]*8      #when education is 8    => 617.2817
const1 + model1$coef[5]*12     #732.2362

const2 <- model2$coef[1] + model2$coef[2]*mean(mroz$kidslt6) + model2$coef[3]*mean(mroz$kidsge6) + model2$coef[4]*mean(mroz$age) + model2$coef[6]*mean(mroz$exper) + model2$coef[7]*mean(mroz$nwifeinc) + model2$coef[8]*mean(mroz$exper2)

(const2 + model2$coef[5]*8)*pnorm((const2 + model2$coef[5]*8)/model2$scale)+(model2$scale*dnorm((const2 + model2$coef[5]*8)/model2$scale))   #replace p2 above to (const2 + model2$coef[5]*8). Since non-linear
                               #423.5725
(const2 + model2$coef[5]*12)*pnorm((const2 + model2$coef[5]*12)/model2$scale)+(model2$scale*dnorm((const2 + model2$coef[5]*12)/model2$scale))
                               #597.6833

#Tobit model shows lower estimates of expected hours worked for these levels of education
#Marginal effects are increasing from education on hours worked
#Change       597.63 - 423     >     732 - 617
