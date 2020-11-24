###### Assignment 8 ##############
## Applying Logistic Regression Analysis

#Getting working directory 
getwd()

#Setting directory to load data set
setwd("/Users/mihikagupta/Desktop/SEM_2/MVA")

#Reading the data into a data frame
#df <- read.csv(file = 'US_Acc_June20.csv')
num <- read.csv(file = 'num.csv')
# Performing clustering on the first 500 records for now to achieve easy and quick results and test the clustering methods
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

# finding covariance,Covariance measures the linear relationship between two variables. ... The correlation measures both the strength and direction of the linear relationship between two variables.
cov(num)
# here we find that the highest covariances with severity in either directions, positive or negative are the first 4 attributes , therefore we select them for our main model


############# TRYING IF LOGISTIC REGRESSION IS A GOOD CHOICE FOR OUR DATASET ################

# Let us first perform a simple multiple regression with some variables
fit<-lm(Severity~dist+temp+windchill+humidity,data = num)

# showing results
summary(fit)
coefficients(fit)

# Performing initial logistic regression on  dataset
# logistic_fit<-glm(Severity~dist+temp+windchill+humidity,data = num,family="binomial")
## When we apply this regression , we get the following error
# Error in eval(family$initialize) : y values must be 0 <= y <= 1

# Alternate Explaination
unique(num$Severity)
# the above result shows that our dependent variable that is "SEVERITY" is not binary , but
# has 4 categories, namesly "1","2","3","4", therefore we cannot use the simple
# binomial logistic regression on this dataset since it is only applicable for binary classification.

############## Computing multinomial logistic regression ############
# therefore we now try the multinomial logistic regression using the "caret" and "nnet"libraries
library("nnet")
library("caret")
library("magrittr")

# Now lets divide our dataset into 2 parts, the training and testing sets for checking our model accuracy later
num1<-num[1:400,]
num2<-num[400:500,]

# applying the multinomial log regression to training set
model<-nnet::multinom(Severity~.,data=num1)

#predicting class of outcome variable
p1<-predict(model,num2,type="class")

# predicting probability of outcome being true
p2<-predict(model,num2,type="probs")

# Rowsums
rowSums(p2)

par(mar=c(1,1,1,1))


plot(Severity ~ dist, col = rgb(0, 0, 0, 0.05), pch = 19,data = num,main="distance vs Severity from 1 to 4")
plot(Severity ~ temp, col = rgb(0, 0, 0, 0.05), pch = 19,data = num,main="temp vs Severity from 1 to 4")
plot(Severity ~ windchill, col = rgb(0, 0, 0, 0.05), pch = 19,data = num,main="windchill vs Severity from 1 to 4")
plot(Severity ~ humidity, col = rgb(0, 0, 0, 0.05), pch = 19,data = num,main="humidity vs Severity from 1 to 4")
plot(Severity ~ pressure, col = rgb(0, 0, 0, 0.05), pch = 19,data = num,main="pressure vs Severity from 1 to 4")
plot(Severity ~ visibility, col = rgb(0, 0, 0, 0.05), pch = 19,data = num,main="visibility vs Severity from 1 to 4")
plot(Severity ~ windspeed, col = rgb(0, 0, 0, 0.05), pch = 19,data = num,main="windspeed vs Severity from 1 to 4")
plot(Severity ~ precip, col = rgb(0, 0, 0, 0.05), pch = 19,data = num,main="precip vs Severity from 1 to 4")
# Summarizing the model
summary(model)
# the above coefficients can now be used to infer how the indivisual variables contribution

# Cannot plot roc curve here since predicted values are not binary

# Making predictions
predicted.classes<-model %>% predict(num2)
head(predicted.classes)


#Checking model accuracy
mean(predicted.classes==num2$Severity)
# we observe that this model predictes the severity with a 67.32 % accuracy


##################### SECOND APPROACH #####################
library("foreign")
library("reshape2")
library(ggplot2)

# performing multinom logistic regression using multinom function
test<-multinom(Severity~.,data=num1)

# printing model summary
summary(test)

z<- summary(test)$coefficients/summary(test)$standard.errors

# performing two tailed z test
p<-(1-pnorm(abs(z),0,1))*2
p

## extract the coefficients from the model and exponentiate
exp(coef(test))

# calculating predicted probabilities for outcome levels using the fitted function
head(pp<-fitted(test))


