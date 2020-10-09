
###### Assignment 4 ##############
## Applying Principal Component Analysis

#Getting working directory 
getwd()
library(pander)
#Setting directory to load dataset
setwd("/Users/mihikagupta/Desktop/SEM_2/MVA")

#Reading the data into a dataframe
df <- read.csv(file = 'US_Acc_June20.csv')

# Printing first few columns of dataset for inference
#head(df)

## Setting random seed to shuflle data before splitting 
set.seed(23)

#Checking number of rows
rows<-sample(nrow(df))

#Shuffling the data
mva<-df[rows,]

#Taking the required number of instances from the shuffled data to reduce any biases
mva<-mva[500000:1000000,]

#Checking the structure of the dataset
str(mva)

# Checking the number of rows and columns in the current uncleaned dataset
ncol(mva)
nrow(mva)

# Printing all the column names to find and filter the relevant and irrelevant attributes
names<-names(mva)
names

## DATA CLEANING ## 

#Dropping the surplus attributes which do not contribute to the analysis
mva <- mva[-c(1:3,7:10,13,14,19,21:23,33,47:49)]

#Checking for any null values in the present dataset
# is.na(mva[,])

#Checking which rows have all the values filled and complete
# complete.cases(mva)

#Making a new dataframe with only the rows that have complete information and all values filled
Mva<-na.omit(mva)
Mva<-Mva[!(is.na(Mva$Sunrise_Sunset) | Mva$Sunrise_Sunset==""), ]
#Mva<- Mva[complete.cases(Mva),]
#Verifying for missing values in the new dataframe
#complete.cases(Mva)
unique(Mva$Sunrise_Sunset)
#Checking the number of rows and columns in the new CLEANED dataframe
ncol(Mva)
nrow(Mva)

# Creating new dataframe with only the numerical attributes to perform statistical functions
num<-Mva[,c(1,4,11:15,17,18)]

##### STEP 1 #######

#Getting the Correlations between the measurements
s<-cor(num)
pander(s)
sum(diag(s))
# Looking at the correlations table, it can be inferred that the attributes have moderate to low correlations in general
# Therefore PCA may not prove to be a much beneficial Method for this Dataset

##### STEP 2 #######

# Using prcomp to compute the principal components
mva_pca<-prcomp(num[,],scale=TRUE)
pander(mva_pca)
pander(summary(mva_pca)$importance)


# sample scores stored in mva_pca$x
# singular values(square roots of eigenvalues) stored in mva_pca$sdev
# loadings (eigenvectors) are stored in mva_pca$rotation
# variable means stored in mva_pca$center
# variable standard deviations stored in mva_pca$scale
# A table containing eigenvalues and %'s accounted, follows
# Eigenvalues are sdev^2

pander(eigen_mva<-mva_pca$sdev^2)
names(eigen_mva)<-paste("PC",1:9,sep="")
pander(eigen_mva)

sum_lambda<-sum(eigen_mva)
pander(sum_lambda)

prop_var<-eigen_mva/sum_lambda
pander(prop_var)

cum_var_mva<-cumsum(prop_var)
pander(cum_var_mva)

mat_lambda<-rbind(eigen_mva,prop_var,cum_var_mva)
rownames(mat_lambda)<-c("Eigenvalues","Prop. variance","Cum. prop. variance")
pander(round(mat_lambda,4))

pander(summary(mva_pca))

pander(mva_pca$rotation)

# Sample scores stored in mva_pca$x
#pander(mva_pca$x)

attach(Mva)
#Identifying the scores by their time of day occurence ( performed wrt to one categorical variable as an example here
#will be performed with all later)
mvatyp_pca<-cbind(data.frame(Sunrise_Sunset),mva_pca$x)
#pander(mvatyp_pca)
#names(mva)

# Means of scores for all the PC's classified by Sunrise_Sunset
tab_meansPC<-aggregate(mvatyp_pca[,2:10],by=list(Sunrise_Sunset=Mva$Sunrise_Sunset),mean)
pander(tab_meansPC)

tab_meansPC <- tab_meansPC[rev(order(tab_meansPC$Sunrise_Sunset)),]
pander(tab_meansPC)

tab_fmeans <- t(tab_meansPC[,-1])
pander(tab_fmeans)

colnames(tab_fmeans) <- t(as.vector(tab_meansPC[1]))
pander(tab_fmeans)
# The third column here represents no specified value of Day or Night, it will be taken care of later as the project progresses

# Standard deviations of scores for all the PC's classified by Sunrise_Sunset

tab_sdsPC <- aggregate(mvatyp_pca[,2:10],by=list(Sunrise_Sunset=Mva$Sunrise_Sunset),sd)
tab_fsds <- t(tab_sdsPC[,-1])

colnames(tab_fsds) <- t(as.vector(tab_sdsPC[1]))
pander(tab_fsds)

t.test(PC1~Mva$Sunrise_Sunset,data=mvatyp_pca)
t.test(PC2~Mva$Sunrise_Sunset,data=mvatyp_pca)
t.test(PC3~Mva$Sunrise_Sunset,data=mvatyp_pca)
t.test(PC4~Mva$Sunrise_Sunset,data=mvatyp_pca)
t.test(PC5~Mva$Sunrise_Sunset,data=mvatyp_pca)
t.test(PC6~Mva$Sunrise_Sunset,data=mvatyp_pca)
t.test(PC7~Mva$Sunrise_Sunset,data=mvatyp_pca)
t.test(PC8~Mva$Sunrise_Sunset,data=mvatyp_pca)
t.test(PC9~Mva$Sunrise_Sunset,data=mvatyp_pca)

# F ratio tests

var.test(PC1~Mva$Sunrise_Sunset,data=mvatyp_pca)
var.test(PC2~Mva$Sunrise_Sunset,data=mvatyp_pca)
var.test(PC3~Mva$Sunrise_Sunset,data=mvatyp_pca)
var.test(PC4~Mva$Sunrise_Sunset,data=mvatyp_pca)
var.test(PC5~Mva$Sunrise_Sunset,data=mvatyp_pca)
var.test(PC6~Mva$Sunrise_Sunset,data=mvatyp_pca)
var.test(PC7~Mva$Sunrise_Sunset,data=mvatyp_pca)
var.test(PC8~Mva$Sunrise_Sunset,data=mvatyp_pca)
var.test(PC9~Mva$Sunrise_Sunset,data=mvatyp_pca)

# Levene's tests (one-sided)

library(car)
(LT_PC1 <- leveneTest(PC1~Mva$Sunrise_Sunset,data=mvatyp_pca))
(LT_PC2 <- leveneTest(PC2~Mva$Sunrise_Sunset,data=mvatyp_pca))
(LT_PC3 <- leveneTest(PC3~Mva$Sunrise_Sunset,data=mvatyp_pca))
(LT_PC4 <- leveneTest(PC4~Mva$Sunrise_Sunset,data=mvatyp_pca))
(LT_PC5 <- leveneTest(PC5~Mva$Sunrise_Sunset,data=mvatyp_pca))
(LT_PC6 <- leveneTest(PC6~Mva$Sunrise_Sunset,data=mvatyp_pca))
(LT_PC7 <- leveneTest(PC7~Mva$Sunrise_Sunset,data=mvatyp_pca))
(LT_PC8 <- leveneTest(PC8~Mva$Sunrise_Sunset,data=mvatyp_pca))
(LT_PC9 <- leveneTest(PC9~Mva$Sunrise_Sunset,data=mvatyp_pca))


# Plotting the scores for the first and second components
par("mar")
par(mar=c(1,1,1,1))
plot(mvatyp_pca$PC1, mvatyp_pca$PC2,xlab="PC1", ylab="PC2", main=" values for PC1 vs  PC2")
abline(h=0)
abline(v=0)
legend("bottomleft", legend=c("Sunrise","Sunset"), pch=c(1,16))
plot(eigen_mva, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")
plot(log(eigen_mva), xlab = "Component number",ylab = "log(Component variance)", type="l",main = "Log(eigenvalue) diagram")
pander(diag(cov(mva_pca$x)))
xlim <- range(mva_pca$x[,1])
plot(mva_pca$x,xlim=xlim,ylim=xlim)

# So after conducting the principal component analysis , we can now interpret the results for dimensionality reduction 
# we can see from the graphs and the scree plots which are the principal components of our dataset amd which ones do not contribute significantly




