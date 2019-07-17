

#Image Compression and Recommendation Systems
#Tool: Singular Value Decomposition (Matrix factorization)

#Image -> Matrix -> Reduced Matrix -> Image (hopefully similar to initial. Perhaps memory size diff)
#Types of images: Grayscale, Color

#Grayscale LKY image
library(jpeg)
lky <- readJPEG("lky.jpg")
str(lky)
min(abs(lky[,,1]-lky[,,2]))               #since gray image, only need one slice, since all the same

View(lky)
s <- svd(lky[,,1])
str(s)                   #d are singular values stored as a vector

#Reducing matrix: approx to rank k. Hopefully if large m and n, small k would suffice

#rank1
lky1 <- s$d[1] * s$u[,1] %*%t(s$v[,1])
writeJPEG(lky1,"lky1.jpg")

#rank 10
lky10 <- s$u[,1:10] %*% diag(s$d[1:10]) %*% t(s$v[,1:10])
writeJPEG(lky10,"lky10.jpg")

#rank 50
lky50 <- s$u[,1:50] %*% diag(s$d[1:50]) %*% t(s$v[,1:50])                 #take top 50 singular values. Impt since subsequent singular values are just details
writeJPEG(lky50,"lky50.jpg")

var <- cumsum(s$d^2)     #adding squares of singular values    
plot(1:410,var)
plot(1:410,var/max(var))

plot(s$d)

#Colored Pansy Image
pansy <- readJPEG("pansy.jpg")                #size 600 x 465 x 3
s1 <- svd(pansy[,,1])                         #performing svd
s2 <- svd(pansy[,,2])
s3 <- svd(pansy[,,3])
str(s1)                       #s1$u:  left singular vectors (600 x 465),  s1$v:   right singular vectors (465 x 465)

pansy50 <- array(dim = c(600,465,3))                                  #create empty array of size 600 x 465 x 3
pansy50[,,1] <- s1$u[,1:50] %*% diag(s1$d[1:50]) %*% t(s1$v[,1:50])   #develop low rank approx of 1st channel, and store in 1st channel of empty array
pansy50[,,2] <- s2$u[,1:50] %*% diag(s2$d[1:50]) %*% t(s2$v[,1:50])   
pansy50[,,3] <- s3$u[,1:50] %*% diag(s3$d[1:50]) %*% t(s3$v[,1:50]) 

writeJPEG(pansy50, "pansy50.jpg") #creates image of size abt 40KB with some blurring

#changing low rank approx of rank 350 to improve image quality

pansy350 <- array(dim = c(600,465,3))                                  
pansy350[,,1] <- s1$u[,1:350] %*% diag(s1$d[1:350]) %*% t(s1$v[,1:350])   
pansy350[,,2] <- s2$u[,1:350] %*% diag(s2$d[1:350]) %*% t(s2$v[,1:350])   
pansy350[,,3] <- s3$u[,1:350] %*% diag(s3$d[1:350]) %*% t(s3$v[,1:350]) 

writeJPEG(pansy350, "pansy350.jpg")  #image of size 45KB, which is quite good representation.


#Using SVD for recommendation systems
#Matrix X  decompose into two matrices: User by Items ->   Users by Factors  +  Factors by Items
#Challenges:
#1. Matrix X very large           -> tackled by gradient descent
#2. Matrix X has many unknown or missing entries

library(recommenderlab)

str(Data)

Data1 <- Data     #define new matrix Data1
Data1[spl1c,spl2c] <- NA
p <- funkSVD(Data1,k=2)           #rank 2 approx
str(p)
Predict <- p$U%*%t(p$V)
Error <- sqrt(mean((Data[spl1c,spl2c]-Predict[spl1c,spl2c])^2,na.rm= TRUE))       #similar to collaborative filtering. 1.11, rather high
#can vary k to get better approx
