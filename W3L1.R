
#Logistic Regression: predicting yes or no, instead of continuous no like runs
#to estimate risk of failure
#key q: chances of oring failure if shuttle launched at 31 fahrenheit?

orings <- read.csv("orings.csv")  #flight, date, field(fail or nv fail), temp, pres
head(orings)
tapply(orings$Field, orings$Flight, sum)    #how many orings failed for ea of the flights
table(tapply(orings$Field, orings$Flight, sum))
plot(orings$Temp[orings$Field>0],orings$Field[orings$Field>0])          #x-axis is temp, y-axis whether got failure. Idea: how temp affects failure
plot(jitter(orings$Temp[orings$Field>0]),orings$Field[orings$Field>0])    #add random noise since points overlap

#havent considered cases where don't fail. more meaningful to use fraction

plot(jitter(orings$Temp),orings$Field)           #significant proportion of nonfailures at high temps

#linear regression model to predict failure, Field
model1 <- lm(Field~Temp + Pres, data=orings)
summary(model1)

model2 <- lm(Field~Temp, data=orings)
summary(model2)
abline(model2)                           #type of linear fit is not good, since data is 0s or 1s.


#logistic regression: response y = [0,1]
# 1 = P(Y=0) + P(Y=1)
# how to predict P(Y=1)
# S-shaped curve: e^x/(1+e^x)   
# model:     P(Y=1)= e^x/(1+e^x)    where x = BO + B1X1 + B2X2 + ... BPXP              #if B +ve, S-shaped. If B -ve, inverse S-shaped
#            P(Y=0)= 1/(1+e^x)
# Odds/how many times more likely:       P(Y=1)/ P(Y=0) = e^x
# log(Odds)                         log(P(Y=1)/P(Y=0)) = x       #if x increases by 1 unit, how it affects log
# max log likelihood
# likelihood: product of probabilities (since want to max these independent observations)
# to make calculations easier,    max log likelihood = max (sum of log of probabilities)
# differentiate and equate to 0
# has to be concave and easy to maximize

model3 <- glm(Field~Temp + Pres, data = orings, family = binomial)
summary(model3)
#null deviance:               how well response var predicted by just intercept
#residual deviance:           how well response var predicted by intercept + additional prediction vars
#significant decrease from null to residual, indicates predictor vars useful
#AIC/Akaike Information Criterion (similar to adjusted R^2):            useful for model selection 
#Smaller AIC, better fit
# no range unlike R^2, since log likelihood no restriction to value
#AIC = 66.47

model4 <- glm(Field~Temp , data = orings, family = binomial)
summary(model4)
#AIC = 66.08
#better model since 1) pres not significant 2) AIC dropped

predict(model4, newdata = orings[144,])
#2.420837 = 6.750 -0.139(31) 
predict(model4, newdata = orings[144,], type="response")
#P(Y=1|temp = 31) = 0.918   #high chance of failure rate

plot(jitter(orings$Temp),orings$Field)
curve(exp(6.75-0.139*x)/(1+exp(6.75-0.139*x)), add=T)
plot(jitter(orings$Temp),orings$Field)
curve(predict(model4,newdata=data.frame(Temp=x),type = "response"),add=T)
