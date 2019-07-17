
model2a <- lm(LPRICE~DEGREES + HRAIN, data=winetrain)
summary(model2a)

fullmodel <- lm(LPRICE~.,data=winetrain)
summary(fullmodel)             #TIME_SV and VINT perfectly correlated, therefore NA

fullmodela <- lm(LPRICE~VINT + DEGREES + HRAIN + WRAIN, data=winetrain)
summary(fullmodela)

cor(winetrain)         #dependent and independent var: want linear correlation (+ve or -ve) to be high, independent and independent var: low correlation (low multicollinearity)
                       #also want to remove insignificant vars one at a time, due to possibility of multicollinearity
confint(fullmodela)
confint(fullmodela, level = 0.99)  #99%confidence for estimates

wineprediction <- predict(fullmodela,winetest)   #predict prices after Y1978
wineprediction               
winetest                       #true values

#want to predict, using diff models, and compare. => Computing test R^2

ssemodela <- sum((winetest$LPRICE[1:2]-wineprediction[1:2])^2)          #(true - predicted value)^2, and sum all up
sstmodela <- sum((winetest$LPRICE[1:2]-mean(winetrain$LPRICE))^2)     #use Ytrain. More mathematically valid
1 - (ssemodela/sstmodela)                                             #test R^2

#model3 <- lm(LPRICE~.,data=winetrain$DEGREES)
#wineprediction3 <- predict(model3,winetest)
#winetest
#hope that better model R^2 leads to better out of sample/test R^2, but may not be the case

#ultimately want good model R^2 and good test R^2


#Moneyball

baseball <- read.csv("baseball.csv")
str(baseball)
baseball2002 <- subset(baseball, baseball$Year <= 2002)   #start of 2002, want to make decisions for upcoming season
str(baseball2002)

#1. goal: to make playoff (95 out of 162 games)
plot(baseball2002$W,baseball2002$Team, col= ifelse(baseball2002$Playoffs==1,"red","black"))
axis(1,at=seq(60,120,by=5))
abline(v=95)
#2. goal: how many runs do we need?   w = 0.1, RD+80.9.  w = 95, RD= (95-80.9)/0.1 = 141
plot(baseball2002$RS-baseball2002$RA,baseball2002$W)
baseball2002$RD <- baseball2002$RS - baseball2002$RA
model1 <- lm(W~RD, data=baseball2002)
summary(model1)
#3. goal: how do you find players who contribute to the wins
m1 <- lm(RS~OBP,data=baseball2002)
summary(m1)

m2 <- lm(RS~SLG,data=baseball2002)
summary(m2)

m3 <- lm(RS~OPS, data=baseball2002)          #OPS = OBP + SLG. By adding up, doesn't tell us which one more impt 
summary(m3)

m4 <- lm(RS~BA, data=baseball2002)
summary(m4)

m5 <- lm(RS~ OBP + SLG, data=baseball2002)   #1 extra unit of OBP, gives more of RS than 1 extra unit for SLG. OBP has more(2x) effect on RS than SLG
summary(m5)

m6 <- lm(RS~ OBP + SLG + BA, data=baseball2002)       #RS = -1011 + 3300 OBP + 1609 SLG, stick to m5 since BA not significant
summary(m6)

m7 <- lm(RA~OOBP + OSLG,data=baseball2002)         #RA = -860 + 2687 OOBP + 1791 OSLG
summary(m7)