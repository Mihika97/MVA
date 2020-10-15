###### Assignment 5 ##############
## Applying Cluster Analysis

#Getting working directory 
getwd()

#Setting directory to load data set
setwd("/Users/mihikagupta/Desktop/SEM_2/MVA")

#Reading the data into a data frame
#df <- read.csv(file = 'US_Acc_June20.csv')
num <- read.csv(file = 'num.csv')
# Performing clustering on the first 500 records for now to achieve easy and quick results and test the clustering methods
num<-num[1:500,]
attach(num)
# Printing first few columns of data set for inference
#head(df)

## Setting random seed to shuffle data before splitting 
set.seed(23)

#Checking number of rows
#rows<-sample(nrow(df))

#Shuffling the data
#mva<-df[rows,]

#Taking the required number of instances from the shuffled data to reduce any biases
#mva<-mva[950000:1000000,]

#Checking the structure of the data set
#str(mva)

# Checking the number of rows and columns in the current uncleaned dataset
#ncol(mva)
#nrow(mva)

# Printing all the column names to find and filter the relevant and irrelevant attributes
#names<-names(mva)
#names

## DATA CLEANING ## 

#Dropping the surplus attributes which do not contribute to the analysis
#mva <- mva[-c(1:3,7:10,13,14,19,21:23,33,47:49)]

#Checking for any null values in the present data set
# is.na(mva[,])

#Checking which rows have all the values filled and complete
# complete.cases(mva)

#Making a new dataframe with only the rows that have complete information and all values filled
#Mva<-na.omit(mva)
#Mva<-Mva[!(is.na(Mva$Sunrise_Sunset) | Mva$Sunrise_Sunset==""), ]
#Mva<- Mva[complete.cases(Mva),]
#Verifying for missing values in the new dataframe
#complete.cases(Mva)
#unique(Mva$Sunrise_Sunset)
#Checking the number of rows and columns in the new CLEANED dataframe
#ncol(Mva)
#nrow(Mva)

# Creating new dataframe with only the numerical attributes to perform statistical functions
#num<-Mva[,c(1,4,11:15,17,18)]
#write.csv(num,"/Users/mihikagupta/Desktop/SEM_2/MVA/num.csv", row.names = FALSE)
#Scaling the dataset before applying clusturing algorithms

nrow(num)
ncol(num)

###### Checking to see for any correlation that might interefere with the data #######
cor(num)

# Removinf the wind chills attribute #
# since "wind chills had a high correlation with temperature, it is removed 
num<-num[-c(4)]

# Checking the new dimensions
nrow(num)
ncol(num)

# Scaling the new data set for better accuracies in clustering
num<-scale(num)

# Determine number of clusters
wss <- (nrow(num)-1)*sum(apply(num,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(num, 
                                     centers=i)$withinss)
plot(wss)
#here we observe that after 8-10 clusters, the distance remains almost constant, so we fix this number as best k value . 

## Loading the cluster library ###
library(cluster)
library(factoextra)

# Understanding different methods

# producing a distance matrix of the dataset
numdist<-dist(num,method = "euclidean")

#Sliced dendrograms for better visualizations

hc <- hclust(dist(num))
hcd <- as.dendrogram(hc)
plot(hcd, main="Main")
plot(cut(hcd, h=5)$upper, 
     main="Upper tree of cut at h=5")
plot(cut(hcd, h=5)$lower[[2]], 
     main="Second branch of lower tree with cut at h=5")

#Single

numsingle<- hclust(numdist, method = "single")
single<-as.dendrogram(numsingle)
plot(single,main="Main")
plot(cut(single,h=1)$upper,xlab="Object",ylab="Distance",
        main="Dendrogram. Nearest neighbor linkage")
plot(cut(single,h=1)$lower[[2]],xlab="Object",ylab="Distance",
     main="Dendrogram. Nearest neighbor linkage")

# Complete link
numcomplete<-hclust(numdist)
plot(numcomplete, hang=-1,xlab="Object",ylab="Distance",
     main="Dendrogram. Farthest neighbor linkage")

# Average linkage
numaverage<-hclust(numdist)
plot(numaverage, hang=-1,xlab="Object",ylab="Distance",
     main="Dendrogram. Group average linkage")

# Single link clustering 
km.res<-kmeans(num,centers = 10,iter.max = 10,nstart = 3)
print(km.res)

# Ward Hierarchical Clustering
fit <- hclust(numdist, method="ward") 
# display dendogram
plot(fit)

# using the agnes function for all options in a single line code
?agnes
# Agglomerative Nesting (Hierarchical Clustering)
(agn.num <- agnes(num, metric="euclidean", stand=TRUE, method = "single"))
#View(agn.num)

#  Description of cluster merging
agn.num$merge

#Dendogram
plot(as.dendrogram(agn.num), xlab= "US Accidents Dendrogram",xlim=c(8,0),
     horiz = TRUE,main="Dendrogram for us accident records")
plot(as.dendrogram(agn.num), xlab= "US Accidents Dendrogram",xlim=c(2,0),
     horiz = TRUE,main="Dendrogram for us accident records")
plot(as.dendrogram(agn.num), xlab= "US Accidents Dendrogram",xlim=c(1,0),
     horiz = TRUE,main="Dendrogram for us accident records")

#Interactive Plots, 
#plot(agn.num,ask=TRUE)
#plot(agn.num, which.plots=2)


#K-Means Clustering


# K-means, k=2, 3, 4, 5, 6
# Centers (k's) are numbers thus, 10 random sets are chosen


# Computing the percentage of variation accounted for. Two clusters
matstd.num<- scale(num[,1:8])
(kmeans2.num <- kmeans(matstd.num,2,nstart = 10))
perc.var.2 <- round(100*(1 - kmeans2.num$betweenss/kmeans2.num$totss),1)
names(perc.var.2) <- "Perc. 2 clus"
perc.var.2

# Computing the percentage of variation accounted for. Three clusters
(kmeans3.num <- kmeans(matstd.num,3,nstart = 10))
perc.var.3 <- round(100*(1 - kmeans3.num$betweenss/kmeans3.num$totss),1)

names(perc.var.3) <- "Perc. 3 clus"
perc.var.3

# Computing the percentage of variation accounted for. Four clusters
(kmeans4.num<- kmeans(matstd.num,4,nstart = 10))
perc.var.4 <- round(100*(1 - kmeans4.num$betweenss/kmeans4.num$totss),1)
names(perc.var.4) <- "Perc. 4 clus"
perc.var.4

# Computing the percentage of variation accounted for. Five clusters
(kmeans5.num <- kmeans(matstd.num,5,nstart = 10))
perc.var.5 <- round(100*(1 - kmeans5.num$betweenss/kmeans5.num$totss),1)
names(perc.var.5) <- "Perc. 5 clus"
perc.var.5


# Computing the percentage of variation accounted for. Six clusters
(kmeans6.num <- kmeans(matstd.num,6,nstart = 10))
perc.var.6 <- round(100*(1 - kmeans6.num$betweenss/kmeans6.num$totss),1)
names(perc.var.6) <- "Perc. 6 clus"
perc.var.6

# Computing the percentage of variation accounted for. Seven clusters
(kmeans7.num <- kmeans(matstd.num,7,nstart = 10))
perc.var.7 <- round(100*(1 - kmeans7.num$betweenss/kmeans7.num$totss),1)
names(perc.var.7) <- "Perc. 7 clus"
perc.var.7

# Computing the percentage of variation accounted for. nine clusters
(kmeans9.num <- kmeans(matstd.num,9,nstart = 10))
perc.var.9 <- round(100*(1 - kmeans9.num$betweenss/kmeans9.num$totss),1)
names(perc.var.9) <- "Perc. 9 clus"
perc.var.9

# Computing the percentage of variation accounted for. ten clusters
(kmeans10.num <- kmeans(matstd.num,10,nstart = 10))
perc.var.10<- round(100*(1 - kmeans10.num$betweenss/kmeans10.num$totss),1)
names(perc.var.10) <- "Perc. 10 clus"
perc.var.10

# Computing the percentage of variation accounted for. twelve clusters
(kmeans12.num <- kmeans(matstd.num,12,nstart = 10))
perc.var.12 <- round(100*(1 - kmeans12.num$betweenss/kmeans12.num$totss),1)
names(perc.var.12) <- "Perc. 12 clus"
perc.var.12

# Computing the percentage of variation accounted for. fifteen clusters
(kmeans15.num <- kmeans(matstd.num,15,nstart = 10))
perc.var.15 <- round(100*(1 - kmeans15.num$betweenss/kmeans15.num$totss),1)
names(perc.var.15) <- "Perc. 15 clus"
perc.var.15

# Saving four k-means clusters in a list
clus1 <- matrix(names(kmeans4.num$cluster[kmeans4.num$cluster == 1]), 
                ncol=1, nrow=length(kmeans4.num$cluster[kmeans4.num$cluster == 1]))
colnames(clus1) <- "Cluster 1"
clus2 <- matrix(names(kmeans4.num$cluster[kmeans4.num$cluster == 2]), 
                ncol=1, nrow=length(kmeans4.num$cluster[kmeans4.num$cluster == 2]))
colnames(clus2) <- "Cluster 2"
clus3 <- matrix(names(kmeans4.num$cluster[kmeans4.num$cluster == 3]), 
                ncol=1, nrow=length(kmeans4.num$cluster[kmeans4.num$cluster == 3]))
colnames(clus3) <- "Cluster 3"
clus4 <- matrix(names(kmeans4.num$cluster[kmeans4.num$cluster == 4]), 
                ncol=1, nrow=length(kmeans4.num$cluster[kmeans4.num$cluster == 4]))
colnames(clus4) <- "Cluster 4"
list(clus1,clus2,clus3,clus4)

# Comparing the effect of number of  clusters
fviz_cluster(kmeans2.num,geom = "point",data=matstd.num)+ggtitle("K=2")
fviz_cluster(kmeans3.num,geom = "point",data=matstd.num)+ggtitle("K=3")
fviz_cluster(kmeans4.num,geom = "point",data=matstd.num)+ggtitle("K=4")
fviz_cluster(kmeans5.num,geom = "point",data=matstd.num)+ggtitle("K=5")
fviz_cluster(kmeans6.num,geom = "point",data=matstd.num)+ggtitle("K=6")
fviz_cluster(kmeans7.num,geom = "point",data=matstd.num)+ggtitle("K=7")
fviz_cluster(kmeans9.num,geom = "point",data=matstd.num)+ggtitle("K=9")
fviz_cluster(kmeans10.num,geom = "point",data=matstd.num)+ggtitle("K=10")
fviz_cluster(kmeans12.num,geom = "point",data=matstd.num)+ggtitle("K=12")
fviz_cluster(kmeans15.num,geom = "point",data=matstd.num)+ggtitle("K=15")




