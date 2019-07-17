
x <- c("yes", "no", "yes", "yes", "no", "maybe", "maybe")
class(x)                  #character
y <- factor(x)
class(y)                  #factor
levels(y)                 #categories
summary(y)
table(x)                  #differs from summary by showing variable

income <- c(1000,3000,500,200,14,25,345)
?tapply              
tapply(income, x,mean)         #average income for ea category

#matrices
m <- matrix(c(3,4,5,6,7,8),nrow =3, ncol=2)     #3 by 2 matrix
m[1,2]                             #row 1, col 2
dim(m)                             #3 2, 3 rows 2 cols
m[4]                               #find 4th element => 6
class(m)

#arrays
n<- array(c(3,4,5,6,7,8),c(3,2))         #array allows to go to more dimensions
class(n)                                 #matrix

z<- 1:50
dim(z) <- c(5,2,5)                      #create 5 by 2 by 5 3 dimension matrix
z[1,1,2]
z[1,1,1]

diag(10)                               #diagonal matrix, 10 ones in diagonal

rbind(c(1,2,3),c(6,7,8))

n[1,1] <- 5
m*n                               

help("%*%")
m%*%t(n)                             #t makes it transpose

a <- array(c(2,1,1,2),c(2,2))
b <- c(4,4)
solve(a,b)                    #2.4 0.8

r <- eigen(a)
r$values                     #eigenvalues
r$vectors                    #eigenvectors

karthil <- list(age = 30, sex ="M", child.ages = c(4,4))
class(karthil)            #list, containing diff objects
karthil$age
karthil$child.ages
karthil$child.ages[1]

sam <- list(age = 25, sex="M", child.ages = NA)
#concatenate just makes it long string. should use concatenating of data frames

d <- data.frame(names = c("karthil","sam","jim"), ages= c(36,40,30), sex= c("M","M","M"))
class(d)
class(d$names)                #factor
class(d$ages)                 #numeric

data()
data(faithful)              #load dataset faithful
faithful
str(faithful)               #tells structure: no of obs, no of vars. Time of eruption, and waiting time
summary(faithful)           #summarize data along columns: min, median, mean
plot(faithful)             #scatterplot of 2 vars
hist(faithful$eruption)
hist(faithful$eruption,breaks=seq(1.6,5.1,0.2))   #some points at the boundary, therefore error
hist(faithful$eruption,breaks=seq(1.6,5.2,0.2))   #bimodal graph
plot(faithful$eruptions)
plot(faithful$eruptions, type="l")
plot.ecdf(faithful$eruptions,type="l")         #plotting cdf 

qqnorm(faithful$eruptions)                   #qqplot: compare data to a distribution eg normal
hist(faithful$eruption,breaks=seq(1.6,5.2,0.2))
qqnorm(faithful$eruptions(faithful$eruptions <=3))      #take out all data that satisfies this, lesser or equal to 3
qqnorm(faithful$eruptions(faithful$eruptions >3))
boxplot(faithful$eruptions)

faithful2 <- subset(faithful, faithful$eruptions > 3)     #get subset of data, based on condition
str(faithful2)

mean(faithfull$waiting)            #sample mean. Can test whether statistically sig. 
                                   # h0: avg waiting time (population mean when eruption) = 0
                                   # h1: avg waiting time when eruption != 0 
t.test(faithfull$waiting)          # get p-value, evidence to reject null hypothesis

library(ggplot2)
w <- read.csv("WHO.csv")
str(w)

which(w$Country =="Singapore")
w[155,]
summary(w$Under15)
summary(w$Over60)
plot(w$GNI,w$FertilityRate)
ggplot(w,aes(x=GNI,y=FertilityRate,)) + geom_point()
ggplot(w,aes(x=GNI,y=FertilityRate,)) + geom_point() + geom_smooth()
ggplot(w,aes(x=GNI,y=FertilityRate, color= Region)) + geom_point()

Crime <- read.csv("Crime.csv")
str(Crime)
head(Crime)
?strptime
Crime$Date[1]
Crime$Date <- strptime(Crime$Date, format = "%m/%d/%y %H:%M")  #data in this format, convert to desired format
Crime$Weekday <- weekdays(Crime$Date)
Crime$Weekday[1]
table(Crime$Weekday)

WeekdayCounts <- as.data.frame(table(Crime$Weekday))
WeekdayCounts

Crime$Hour <- Crime$Date$hour
Crime$Hour[1]

WeekdayHourCounts <- as.data.frame(table(Crime$Weekday,Crime$Hour))
str(WeekdayHourCounts)
WeekdayHourCounts$Var1
WeekdayHourCounts$Var1 <- factor(WeekdayHourCounts$Var1, ordered= TRUE, levels = c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday", "Sunday"))
WeekdayHourCounts$Var2 <- as.numeric(as.character(WeekdayHourCounts$Var2))  #put char, otherwise factors go from 0 to 1
ggplot(WeekdayHourCounts, aes(x=Var2,y=Var1))+ geom_tile(aes(fill=Freq)) + xlab("Hour of day") + ylab("")