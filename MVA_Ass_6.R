###### Assignment 6 ##############
## Applying Factor and Component Analysis

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

# Creating new dataframe with only the numerical attributes to perform statistical functions
#num<-Mva[,c(1,4,11:15,17,18)]
#write.csv(num,"/Users/mihikagupta/Desktop/SEM_2/MVA/num.csv", row.names = FALSE)

# Scaling the new data set for better accuracies 
# num<-scale(num)

# Checking the dimensions of the data
nrow(num)
ncol(num)

###### Computing correlation matrix #######
cor.num<-cor(num)
cor.num
par(mar=c(1,1,1,1))
plot(cor.num,main ="plotting correlation" )

########## Applying FA ##########################################

# applying basic component analysis
num_pca<-prcomp(num[-1],scale=TRUE)
summary(num_pca)
plot(num_pca)

# A table containing eigenvalues and %'s accounted, follows.
# Eigenvalues are the square of sdev
eigen_num<-round(num_pca$sdev^2,2)
names(eigen_num) <- paste("PC",1:8,sep="")
eigen_num

# sum of eigen values
sumlambdas<-sum(eigen_num)
sumlambdas

# cumulative variance
propvar<-round(eigen_num/sumlambdas,2)
propvar
cumvar_num<-cumsum(propvar)
cumvar_num

# matrix representation
matlambdas<-rbind(eigen_num,propvar,cumvar_num)
rownames(matlambdas)<-c("Eigen Values","Prop variance","Cum prop variance")
rownames(matlambdas)
matlambdas

# finding rotation
eigvec.num<-num_pca$rotation
print(num_pca)
eigvec.num

# Taking the first 4 PCs to generate linear combinations for all the variables with four factors
pcafactors.num<-eigvec.num[,1:4]
pcafactors.num

# Multiplying each column of the eigenvector's matrix by the square root of 
# the corresponding eigenvalue in order to get the factor loadings
unrot.fact.num<-sweep(pcafactors.num,MARGIN=2,num_pca$sdev[1:4],`*`)
unrot.fact.num

# Computing communalities
communalities.num<-rowSums(unrot.fact.num^2)
communalities.num

# Performance the varimax rotation. The default in the varimax function is norm=TRUE
# thus Kaiser normalization is carried out
rot.fact.num<-varimax(unrot.fact.num)
# View(unrot.fact.num)
rot.fact.num

# The print method of varimax omits loadings less than abs(0.1) in order to display all the loadings
# it is necessary to ask explicitly the contents of the object$loadings
fact.load.num<-rot.fact.num$loadings[1:8,1:4]
fact.load.num

# Computing the rotated factor scores for the us accident instances
# Notice that the signs 
scale.num<-scale(num[-1])
scale.num

as.matrix(scale.num)%*%fact.load.num%*%solve(t(fact.load.num)%*%fact.load.num)

# using the psyche package
library(psych)

fit.pc<-principal(num[-1],nfactors=4,rotate = "varimax")
fit.pc

# fit loadings
round(fit.pc$values,3)
fit.pc$loadings

# Loadings with more digitd
for(i in c(1,2,3,4))
  {
  print(fit.pc$loadings[[1,i]])
}

# Communalities
fit.pc$communality

# Rotated factor scores, Notice certain peculiarities in columns
fit.pc$scores


# Other FA Utilities
fa.parallel(num[-1]) # Factor recommendation
fa.plot(fit.pc) # Correlation within factors
fa.diagram(fit.pc) # Visualizing the relationship
vss(num[-1]) # Factor recommendation for a simple structure




