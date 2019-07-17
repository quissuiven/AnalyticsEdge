
#Recommendation Systems
#1. Clustering techniques 2. Collaboration and content filtering
#Goal: Minimize root squared mean error

movies <- read.csv("movies.csv",stringsAsFactors = FALSE)
head(movies)
countfields <- count.fields("genres.csv", sep ="|")
head(countfields)            #how many genres a movie lies in
min(countfields)
max(countfields)
str(movies)                  

genres <- read.csv("genres.csv", header = FALSE, sep = "|", col.names = c("X1","X2","X3","X4","X5","X6","X7"))
head(genres)
head(movies)      #want to move movie titles to column titles of data set
levels(genres$X1)
levels(genres$X2) 
fac <- union(union(union(union(union(union(levels(genres$X1),levels(genres$X2)),levels(genres$X3)),levels(genres$X4)),levels(genres$X5)),levels(genres$X6)),levels(genres$X7))    #since union only works wiht 2 args. To standardize factor var for all columns

genres$X1 <- factor(genres$X1, fac)
genres$X2 <- factor(genres$X2, fac)
genres$X3 <- factor(genres$X3, fac)
genres$X4 <- factor(genres$X4, fac)
genres$X5 <- factor(genres$X5, fac)
genres$X6 <- factor(genres$X6, fac)
genres$X7 <- factor(genres$X7, fac)

levels(genres$X1)
levels(genres$X7) 

M <- matrix(0, nrow = 8569, ncol = 20)       #since 8569 movies
colnames(M) <- fac
for(i in 1:8569){
M[i,genres[i,"X1"]] <- 1                 #For ea movie, if action in my first col put 1, 0 otherwise
M[i,genres[i,"X2"]] <- 1                 #genres all now all cols
M[i,genres[i,"X3"]] <- 1 
M[i,genres[i,"X4"]] <- 1 
M[i,genres[i,"X5"]] <- 1 
M[i,genres[i,"X6"]] <- 1 
M[i,genres[i,"X7"]] <- 1 
}

head(M)

Data <- as.data.frame(M)
Data$title <- movies$title
head(Data)
Data <- Data[,-19]    #drop 19th col, since empty entry


#CLUSTERING

#want to cluster movies who are similar together
#not as easy to evaluate quality of cluster. Since no truth to compare that, it's subjective

#1. Define Distance between 2 points
#   > using Centroid distance:   Take average of data points into one middle data point. Compute Euclidean distance btw 2 middle data points


#K-MEANS CLUSTERING:
#1. Specify number of clusters, K
#2a. Assignment step:   Assign ea obs to the cluster whose mean is closest
#2b. Update step: Calculate new mean of observations in new cluster
#create a partition C1, C2... CK. Clusters cannot overlap
#obj: Minimize within cluster sum of squares
# compute random centres, allocate obs to closest cluster, compute avg of points. Recompute centres

set.seed(1)
clusterMovieskmeans <- kmeans(Data[,1:19],centers=10,nstart=20)          #use first 19 columns, choose 10 clusters instead of 2^19, run clusters with 20 random diff starting points
clusterMovieskmeans$tot.withinss                 #for each cluster, what is sum of squared distance. Want to minimize this
clusterMovieskmeans$size
clusterMovieskmeans$centers     #row: cluster no, col: variable
t(clusterMovieskmeans$centers)          #nos: what proportion of an attribute inside a cluster

#as no of clusters increases, error in cluster fit decreases
fit <- 0
set.seed(1)
for (k in 1:15){       #for ea cluster
  clusterMovieskmeans1 <- kmeans(Data[,1:19],centers=k,nstart=20)               #as no of cluster increases, fit goes down
  fit[k] <- clusterMovieskmeans1$tot.withinss
}
plot(1:15,fit)

Cat2 <- matrix(0, nrow = 19, ncol = 10)
for (i in 1:19){
  Cat2[i,]<-tapply(Data[,i],clusterMovieskmeans$cluster, mean)              #divides data points into 10 clusters, computes avg value for each cluster or % of movies belonging to that cluster    
}
rownames(Cat2) <- colnames(Data)[1:19]




#HIERARCHICAL CLUSTERING:  no need k to be pre-specified
#Lecture notes
#1. Begin w n observations, for ea pair of obs compute pairwise dissimilarities (within cluster variance)
#2a. Identify pairs of clusters most similar. Fuse these clusters.
#2b. Compute pairwise dissimilarities among remaining clusters

#EdX 
#Combine 2 nearest clusters into one cluster repeatedly based on Euclidean and Centroid distances
#Dendrogram: 
#height represents distance (dissimilarity) between points or clusters. Larger height more dissimilar
#          Fusion into a branch corresponds to similar obs
#          > obs that fuse at top of tree: very different
#          > obs that fuse at btm of tree: very similar
#Determining no of clusters:  Draw a horizontal line across. No of clusters = no of vertical lines crossed
#1. compute distances  (using dist function)
#2. cluster the points  (hclust: ward method cares abt distance btw clusters using centroid distance, and variance in ea cluster)
distances <- dist(Data[,1:19],method = "euclidean")
clusterhi <- hclust(distances,method ="ward.D2" )              
plot(clusterhi)                                           #plot dendrogram
clustergrouphi <- cutree(clusterhi, k=10)           #cuts tree resulting from hclust into 10 groups
str(clustergrouphi)

tapply(Data[,1], clustergrouphi, mean)              #compute avg value across cluster groups for Action var. Higher values means many movies in cluster are Action movies

Categories <- matrix(0, nrow = 19, ncol = 10)
for (i in 1:19){
  Categories[i,]<-tapply(Data[,i],clustergrouphi, mean)              #divides data points into 10 clusters, computes avg value for each cluster or % of movies belonging to that cluster    
}
rownames(Categories) <- colnames(Data)[1:19]   #create matrix categories where rows denote categories, columns indicate clusters

#take subset of movies
subset(Data$title, clustergrouphi == 6)  #lists out all movies in category 6
subset(Data, Data$title == "Grand Budapest Hotel, The(2014")


