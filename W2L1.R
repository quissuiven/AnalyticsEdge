

install.packages("maps")
library(maps)
install.packages("ggmap")
library(ggmap)

chicago <- get_map(location = "chicago")
ggmap(chicago)
head(Crime)                                                    #return first or last part of vector/matrix/data frame

table(round(Crime$Longitude,2),round(Crime$Latitude,2))        #for ea longtitude, latitude, how many crimes

LatLongCount = as.data.frame(table(round(Crime$Longitude,2),round(Crime$Latitude,2)))
head(LatLongCount)
class(LatLongCount$Var1)                      #factor
LatLongCount$Var1 <- as.numeric(as.character(LatLongCount$Var1))
class(LatLongCount$Var1)                        #numeric
head(LatLongCount)

LatLongCount$Var2 <- as.numeric(as.character(LatLongCount$Var2))
#show what crimes occur at which rate
ggmap(chicago) + geom_tile(data=LatLongCount, aes(x=Var1, y = Var2, fill = Freq), fill="red")
ggmap(chicago) + geom_tile(data=LatLongCount, aes(x=Var1, y = Var2, alpha = Freq), fill="red") 


#Q: How to predict quality of the wine when it matures?
#Use simple linear regression model to predict quality and price of wine
#Predictors: Vineyard (location), Vintage (year wine is made)   ->  Quality of Wine
wine <- read.csv("wine.csv")
str(wine)                       #contains VINT(vintage year), LPRICE(log of price):what we want to predict/ dependent/output/predicted
                                #WRAIN(Rain during winter), DEGREES(Summer temperature), HRAIN(Harvest rain)
is.na(wine)             #check where missing entries
plot(wine$VINT,wine$LPRICE)            #older wines higher price, more valued
pairs(wine)                            #does scatterplot w every other var

#dataset split into train and test,  training set to build model and check w test set
winetrain <- subset(wine, wine$VINT <= 1978 & !is.na(wine$LPRICE))           #take a subset of data frame, for these 2 conds, excluse na values
str(winetrain)
winetest <- subset(wine, wine$VINT > 1978) 
str(winetest)

?lm   #fit a linear model
model1 <- lm(LPRICE~VINT, data=winetrain)
model1
summary(model1) #Intercept/B0 = 72.99, VINT = -0.03786

#want to check if B0, B1 significant predictor of price
#using t value = B estimate/ std. error
#stars, typically show significant. Reject null hypothesis. VINT is significant predictor of output
#SSE = sum(model$residuals^2)
#R^2 = 1 - SSE/SST   a good measure comparing with SSE of baseline model, captures value added from using a model.
#R^2 = 0 means no impvt over baseline, R^2 = 1 means perfectly predictive model
#adjusted R^2 keeps track of how many vars we're using (+ B2X2), does it benefit?

plot(wine$VINT, wine$LPRICE)
plot(winetrain$VINT, winetrain$LPRICE)
abline(model1)
model1$residuals

sst1 <- sum((winetrain$LPRICE-mean(winetrain$LPRICE))^2)
sst1
sse1 <- sum(model1$residuals^2)
rsq = 1 - sse1/sst1
rsq
summary(model1)
abline(a= mean(winetrain$LPRICE),b= 0)

model2 <- lm(winetrain$LPRICE~winetrain$WRAIN)            #1 var WRAIN not sig. R^2 = 0.08. Thus, model 1 > model 2
summary(model2)

model3 <- lm(winetrain$LPRICE~winetrain$DEGREES)          #DEGREES sig, R^2 = 0.435. Thus, model 3 > model 1
summary(model3)

model4 <- lm(winetrain$LPRICE~winetrain$HRAIN)
summary(model4)

plot(winetrain$DEGREES,winetrain$HRAIN)
abline(h = mean(winetrain$HRAIN))
abline(v = mean(winetrain$DEGREES))
plot(winetrain$DEGREES, winetrain$HRAIN, col=ifelse(winetrain$LPRICE>mean(winetrain$LPRICE), "red", "black"))  #if price above avg, color red. otherwise, black
abline(h = mean(winetrain$HRAIN))
abline(v = mean(winetrain$DEGREES))
