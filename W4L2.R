

table(oscars$PrNl[oscars$MM==1],oscars$Ch[oscars$MM==1])


#building multinomial logit model 

library("mlogit")

#create dataframes for best pic, best director, best male actor, best female actor
oscarsPP <- subset(oscars, PP==1)                          #best pic subset
oscarsDD <- subset(oscars, DD==1)
oscarsMM <- subset(oscars, MM==1)
oscarsFF <- subset(oscars, FF==1)

oscarsPP$GG <- oscarsPP$Gdr + oscarsPP$Gmc                      #define new var that captures if movie won golden globe for best pic

#shape data frame for use in mlogit model
D <- mlogit.data(subset(oscarsPP, Year <= 2006),choice ="Ch", shape = "long",alt.var="Mode" )              
          #ch captures output, 
          #long if each row is an alternative, wide if each row is an observation
          #indicate Mode, as it captures all the values of alternatives
          
Dtest <- mlogit.data(subset(oscarsPP, Year <= 2006),choice ="Ch", shape = "long") 
Dtest

M <- mlogit(Ch~Nom+Dir+Aml+Afl+GG+PGA+Length+Days-1, data = D)        #days -1, to exclude beta0, the intercept, for this particular case
summary(M)                                 #more nominations, increase likelihood of winning (since significant)
                                           #returns max likelihood

        #x is nom/dir/aml/afl ..., then want to find B coeff
M1 <- mlogit(Ch~Nom+Dir+GG+PGA-1, data = D)       #remove var, max likelihood decreases. Negative due to log
summary(M1)

#build relative model, accounted for by denominator, which will adjust relative to competitors each year
#probability of winning will always add up to 1

#testing quality of fit
#1) for each model, compute AIC = -2LL + 2(no of parameters)               *(no of parameters + 1) if theres an intercept

#Model M = -2(-36.722) +2(8) = 89.44
#Model M1 = -2(-38.171) + 2(4) = 84.342(better)

#2) Likelihood ratio index = 1 - LL(B)/LL(o)         => small value indicates close to random choice (not good). LL(B) close to LL(0)
                                                    #=> large value (close to 1) indicates perfect prediction,  LL(B) close to 0
285/5        #57
1 - (-38.171/(log(0.2)*56))   #56 years

#Predict out of sample winners for beyond 2006/ year 2007
D1 <- mlogit.data(subset(oscarsPP, Year > 2006),choice ="Ch", shape = "long",alt.var="Mode" )  
P1 <- predict(M1, newdata = D1)            #probabilities of each nominee winning

subset(oscarsPP, Year == 2007)            #No country for old: winner for Oscars 2007 best picture

#Surprise winners
D <- mlogit.data(oscarsPP,choice ="Ch", shape = "long",alt.var="Mode" )  
M <- mlogit(Ch~Nom+Dir+GG+PGA-1, data = D)  
P <- predict(M, newdata = D)                 #Predict on Best Pictures dataset probabilities ea nominee will win
PredProb <- as.vector(t(P))                    #transpose and convert ea col
oscarsPP$Pred <- PredProb
oscarsPP$Pred[oscarsPP$Ch==1]                #Probabilities of predictions that won
subset(oscarsPP, Year == 2004)               #Million Dollar Baby won Best Picture, even tho predicted probability of 0.02

#for best male actor, wan to see coefficients over time
oscarsMM <- subset(oscars, MM == 1)      
Fail <- 0
Predict <- NULL
Coefficients <- NULL          #keep track of beta coefficients over time

summary(oscarsMM$Year)
for(i in 1960:2006){
      D <- mlogit.data(subset(oscarsMM, Year <= i),choice ="Ch", shape = "long",alt.var="Mode" )      
      M <- mlogit(Ch~Pic+Gm1+Gm2+PrNl+PrWl-1, data = D)
      Coefficients <- rbind(Coefficients,M$coeff)
      D1 <- mlogit.data(subset(oscarsMM, Year == i+1),choice ="Ch", shape = "long",alt.var="Mode" )  
      P1 <- predict(M, newdata=D1)
      Predict <- rbind(Predict,P1)
      Fail <- Fail + as.logical(which.max(P1)-which.max(subset(oscarsMM, Year == i+1)$Ch))   #which year has max probability. as.logical makes it 1 or 0
}

Coefficients

Fail   #14

43/67

#limitation of multinomial logit model: inaccurate prediction of items similar to one another (red bus and blue bus)
#for oscars, it's ok since movies are different