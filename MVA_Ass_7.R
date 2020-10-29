###### Assignment 7 ##############
## Applying Multiple Regression Analysis

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

# Performing multiple regression on  dataset
fit<-lm(Severity~dist+temp+windchill+humidity,data = num)

# showing results
summary(fit)
coefficients(fit)

# plotting the scatterplot matrix
library(ggplot2)
require(GGally)
# ggpairs(data=num,title = "US Accidents Data")

# confidence levels
confint(fit,level=0.95)

# Predicted Values
fit_values<-fitted(fit)
res<-residuals(fit)

# ANOVA Table
anova(fit)
vcov(fit)
cov2cor(vcov(fit))

# Measure influence 
temp <- influence.measures(fit)
# View(temp)

# Assessing Outliers
library(car)
outlierTest(fit)
par(mar=c(1,1,1,1))
qqPlot(fit, main="QQ Plot")
leveragePlots(fit) # leverage plots

# Influential Observations
# added variable plots
avPlots(fit)

# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(num)-length(fit$coefficients)-2))
plot(fit, which=4, cook.levels=cutoff)

# Influence Plot
influencePlot(fit, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

# Normality of Residuals
# qq plot for studentized resid
qqPlot(fit, main="QQ Plot")

# distribution of studentized residuals
library(MASS)
sresid <- studres(fit)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)

#Non-constant Error Variance
# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(fit)

# plot studentized residuals vs. fitted values
spreadLevelPlot(fit)

#Multi-collinearity
# Evaluate Collinearity
vif(fit) # variance inflation factors
sqrt(vif(fit)) > 2 # problem?

#Nonlinearity
# component + residual plot
crPlots(fit)
# Ceres plots
#ceresPlots(fit)

#Non-independence of Errors
# Test for Autocorrelated Errors
durbinWatsonTest(fit)

# Global test of model assumptions
library(gvlma)
gvmodel <- gvlma(fit)
summary(gvmodel)
fit
summary(fit)
fit1 <- fit
fit2 <- lm(Severity ~ dist+temp+windchill, data = num)

# compare models
anova(fit1, fit2)
step <- stepAIC(fit, direction="both")
step$anova # display results
library(leaps)
leaps<-regsubsets(Severity~dist+temp+windchill+humidity+precip,data=num,nbest=10)

# view results
summary(leaps)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leaps)
plot(leaps,scale="r2")
# subsets(leaps, statistic="rsq")

# All Subsets Regression
#plot(leaps,scale="bic")
#summary(leaps)
#?regsubsets
#summary(leaps)
#View(leaps)
#leaps
#coef(leaps,1:5)



