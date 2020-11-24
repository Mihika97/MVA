###### Assignment 9 ##############
## Applying Linear Discriminant Analysis

#Getting working directory 
getwd()

#Setting directory to load data set
setwd("/Users/mihikagupta/Desktop/SEM_2/MVA/Assignments")

#Reading the data into a data frame
#df <- read.csv(file = 'US_Acc_June20.csv')
num <- read.csv(file = 'num.csv')
# Performing analysis on the first 500 records for now to achieve easy and quick results and test the discriminant methods
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
names(num)

names(num)[names(num) == "Distance.mi."] <- "dist"
names(num)[names(num) == "Temperature.F."] <- "temp"
names(num)[names(num) == "Wind_Chill.F."] <- "windchill"
names(num)[names(num) == "Humidity..."] <- "humidity"
names(num)[names(num) == "Pressure.in."] <- "pressure"
names(num)[names(num) == "Visibility.mi."] <- "visibility"
names(num)[names(num) == "Wind_Speed.mph."] <- "windspeed"
names(num)[names(num) == "Precipitation.in."] <- "precip"
names(num)

############# LDA ###################
library(MASS)

head(num)
num.data<-as.matrix(num)

# Splitting the dataset into two parts
smp_size_raw<-floor(0.75*nrow(num.data))
train_ind_raw<-sample(nrow(num.data),size = smp_size_raw)
train_raw.df<-as.data.frame(num.data[train_ind_raw,])
test_raw.df<-as.data.frame(num.data[-train_ind_raw,])
# we now have a training and test set .Training is 75% and test is 25%

## Applying lda
num.lda<-lda(formula=train_raw.df$Severity~.,data = train_raw.df)
num.lda
summary(num.lda)

# general parameters in lda
num.lda$counts
num.lda$means
num.lda$scaling
num.lda$prior
num.lda$lev
num.lda$svd

print(num.lda)
plot(num.lda)

#predicting the classes
num.lda.predict<-predict(num.lda,newdata = test_raw.df)
num.lda.predict$class
# View(num.lda.predict)
num.lda.predict$x

# Getting the posteriors as a dataframe
num.lda.predict.posteriors<-as.data.frame(num.lda.predict$posterior)
library("ROCR")

# creating the ROC/AUC curve
# pred<-prediction(num.lda.predict.posteriors[,4],test_raw.df$Severity)
# ROCR not working here since it supports evaluation of only binary classification taska as of now

#singular values (svd) that gives the ratio of the between- and within-group standard deviations on the linear discriminant variables.
class(num.lda)
num.lda$N
num.lda$call

(prop=num.lda$svd^2/sum(num.lda$svd^2))

#we can use the singular values to compute the amount of the between-group variance that is explained by each linear discriminant.
num.lda2<-lda(formula=train_raw.df$Severity~.,data = train_raw.df,CV=TRUE)
num.lda2

head(num.lda2$class)

#the Maximum a Posteriori Probability (MAP) classification (a factor)
#posterior: posterior probabilities for the classes.

head(num.lda2$posterior,3)

# Partition plots
library("klaR")
#partimat(Severity~.,data=train_raw.df,method="lda")











