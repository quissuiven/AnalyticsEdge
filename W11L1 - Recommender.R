

#Recommendation Systems
#2 main types: 1) Collaborative filtering 2) Content filtering
#Will focus on collaborative filtering for this class
#need past data (users' ratings) to compare similarities.  Use their predictions to make predictions
 
#baseline model 1: avg rating of movie i across all users who rated it
#baeline model 2: avg rating of all movies rated by user u

#User-based collaborative filtering
#1. Find users similar to "u"  - using Pearson correlation
#2. Choose an appropriate sized set of similar users
#3. Predict based on average rating of these users

ratings <- read.csv("ratings.csv")
str(ratings)
min(table(ratings$userId))          #users rated 20 movies min
max(table(ratings$userId))
min(table(ratings$movieId))         
max(table(ratings$movieId))
    
Data <- matrix(nrow = length(unique(ratings$userId)),ncol = length(unique(ratings$movieId)))    #each row represents single user, each col represents single movie
str(Data)
100023/ (8552 * 706)       #1.65%. Extremely sparse matrix

rownames(Data) <- unique(ratings$userId)
colnames(Data) <- unique(ratings$movieId)
for (i in 1:nrow(ratings)){                    #populate entries in matrix w actual rating user gave
  Data[as.character(ratings$userId[i]),as.character(ratings$movieId[i])] <- ratings$rating[i]
}
str(Data)



#spl2 is my preferences. Want to find data in spl1 that is similar to my preferences (using correlation), to make predictions

set.seed(1)
spl1 <- sample(1:nrow(Data),0.98*nrow(Data))      #doing partition. Designating parts of the matrix 
head(spl1)

set.seed(2)
spl2 <- sample(1:ncol(Data),0.8*ncol(Data))             
spl1c <- setdiff(1:nrow(Data),spl1)                              #goal: for users in sp1c, want to predict sp2c movies
spl2c <- setdiff(1:ncol(Data),spl2)
str(spl2c)         #1711 obs

#baseline model

Base1 <-matrix(nrow =length(spl1c),ncol = length(spl2c))
Base2 <-matrix(nrow =length(spl1c),ncol = length(spl2c))

for (i in 1:length(spl1c)){
  Base1[i,] <- colMeans(Data[spl1,spl2c], na.rm= TRUE)            #use avg for spl1 to make prediction for spl2c
}
for (j in 1:length(spl2c)){
  Base2[,j] <- rowMeans(Data[spl1c,spl2],na.rm= TRUE)
}

RMSEBase1 <- sqrt(mean((Base1 - Data[spl1c,spl2c])^2,na.rm = TRUE))   #0.931
RMSEBase2 <- sqrt(mean((Base2 - Data[spl1c,spl2c])^2,na.rm = TRUE))   #0.995


#creating user predictions
User <-matrix(nrow =length(spl1c),ncol = length(spl2c))
Cor <-matrix(nrow =length(spl1),ncol = 1)                      #keep track of correlation among users
Order <- matrix(nrow = length(spl1c),ncol=length(spl1))        #keep track of which users similar, which are not

for(i in 1:length(spl1c)){
  for (j in 1:length(spl1)){
      Cor[j] <- cor(Data[spl1c[i],spl2],Data[spl1[j],spl2],use="pairwise.complete.obs")     #correlate data only on the left side of matrix, should have 691 correlations
  }
  v <- order(Cor, decreasing = TRUE, na.last = NA)                #order correlations from high to low
  Order[i,] <- c(v, rep(NA,times = length(spl1)-length(v)))
}

for(i in 1:length(spl1c)){
  User[i,] <- colMeans(Data[spl1[Order[i,1:250]],spl2c],na.rm = TRUE)            #chosen 250 closest users as rows in spl1, use data for predictions on spl2c. Drop missing entries
}
str(User)

RMSEUser <- sqrt(mean((Data[spl1c,spl2c]-User)^2,na.rm=TRUE))        #0.898, smaller than baseline model

RMSE <- rep(NA, times=490)
for(k in 10:499){                     #10 users to 499 users
      for (i in 1:length(spl1c)){
        User[i,]<- colMeans(Data[spl1[Order[i,1:k]],spl2c],na.rm = TRUE) 
      }
      RMSE[k] <- sqrt(mean((Data[spl1c,spl2c]-User)^2,na.rm=TRUE))
}                            

head(RMSE)
RMSE[10]
RMSE[11]
plot(10:499, RMSE[10:499])             #drop in RMSE, as set size increases. But at some point, too many neighbours, RMSE increases
abline(h=0.91)
