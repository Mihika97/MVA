
###### Assignment 3 ##############

## EDA
## Data Cleaning 
## Tests 

#Getting working directory 
getwd()

#Setting directory to load dataset
setwd("/Users/mihikagupta/Desktop")

#Reading the data into a dataframe
df <- read.csv(file = 'US_Acc_June20.csv')

# Printing first few columns of dataset for inference
head(df)


## DATA EXPLORATION AND REDUCTION ##

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

#Verifying for missing values in the new dataframe
#complete.cases(Mva)

#Checking the number of rows and columns in the new CLEANED dataframe
ncol(Mva)
nrow(Mva)


## EXPLORATORY DESCRIPTIVE STATISTICS ##

#Getting some basic initial statistical values for insights into the dataset
summary(Mva)

#Checking for datatypes of each of the attributes in the dataset
sapply(Mva,class)

# Creating new dataframe with only the numerical attributes to perform statistical functions
num<-Mva[,c(1,4,11:15,17,18)]

# Finding the correlation between all numerical variables
corr<-cor(num,method=c("pearson","kendall","spearman"))
corr

# OR

#Plotting the correlation graph
library(corrplot)
plot(corr)
#We observe that there are strong correlations between Temp and visibility, wind chill and visibility,
#Humididty and pressure etc, strong neg corrs are observed between Humidity and wind chill for instance
# A higher pos value between 0 and 1 is positive corr and neg value between 0 and -1 is a neg corr



# DATA EXPLORATION AND VISUALIZATION  ##

#Ques 1. How are the numerical weather variables related to the KPI Severity of accident ??

## Plotting Scatter plots between each variable and the KPI(Severity)
plot(Mva$Distance.mi.,Mva$Severity) 
#Some outliers are found in the distance column for distances larger than 20 mtrs, most categories of severities have taken place within the range of 20 mtrs

plot(Mva$Temperature.F.,Mva$Severity)
#From the apparent density of the data points on the severity vs temp plot, for slight rise in temp the severity has increased maybe because people are speeding up as weather is less chilly and more sunny

plot(Mva$Wind_Chill.F.,Mva$Severity) 
#A similar effect is observed , as wind chills temp rises severity is increasing , also no category 1 accidents are obs when wind chill is less than 0 degrees

plot(Mva$Humidity...,Mva$Severity)
#Humidity seems to almost have no co relation with severity here 

plot(Mva$Pressure.in.,Mva$Severity)
#No cat 1 accidents for pressures less than 23 inches, for other categories , the data seem pretty skewed with outliers and needs to be scaled

plot(Mva$Visibility.mi.,Mva$Severity)
# Almost all categories of accidents have taken place when Visibility was 0, with few outliers in each category

plot(Mva$Wind_Speed.mph.,Mva$Severity)

plot(Mva$Precipitation.in.,Mva$Severity)

#Scatterplots here show how the variables are distributed and their frequencies in each category of severity

#Ques 2. How are the attributes distributed, what values or ranges of each numerical attribute is more prevalent??

#Plotting histograms
library(ggplot2)

#Which was the category that most accidents fell under??
hist(Mva$Severity)
# Category 2 severity was most prevalent

#What was the distance distribution over which the accident took place??
hist(Mva$Distance.mi.)
#Mostly between 0 to 20 meters

# What temp contributed most to the accidents??
hist(Mva$Temperature.F.)
# Most accidents took place when the temp was between 60-80 deg F

# How did wind chill contribute to the accident??
hist(Mva$Wind_Chill.F.)
#Most accs took place with wind chill in the range 50-100

# Contribution of Humidity??
hist(Mva$Humidity...)
#Shows a strong positive trend , as the humidity increased so did the number of accidents

#Pressure contribution??
hist(Mva$Pressure.in.)
#Again we see a positive relation, as the higher values of pressures show higher numbers of accidents

#Visibility vs no of accidents??
hist(Mva$Visibility.mi.,xlim=c(0,12),breaks=200)
# A low visibility  is the most prevalent for accident and thus has much more accidents associated with it
#AS visibility increased accidents decreased

#Was Wind speed a factor in accident event??
hist(Mva$Wind_Speed.mph.,xlim=c(0,75),breaks=500)
#Wind chills seem to be negatively related to accidents, maybe when high wind chills were present , less people chose to drive and get outdoors

#Effect of precipitation?
hist(Mva$Precipitation.in.,breaks=200,xlim=c(0,5))
#Precipitation shows no significant relation to no of accidents

# Histograms here show the frequencies of each attribute within a certain range, 

#Finding covariance and mahalanobis, Euclidean distance
cov(num)
#Again here we find strong positive relationships between Temp and wind chill,Temp and visibiltiy etc
#Strong neg relation between Humidity and visibility 
# For the KPI that is severity, Humidity, wind speed,Distance were the attributes that showed slightly positive covariances
#dist(num,method = "euclidean")
#mahalanobis(x=Mva$Distance.mi.,data.y=Mva$Temperature.F.)

## FINDING NORMALITY OF DATA ATTRIBUTES (NUMERICAL) ##

#Finding qqnorm normality in dataset, similarly will be plotted for all numerical attributes,(finding normality in dataset)

qqnorm(num[,c("Visibility.mi.")],main="QQ Norm plot for Visibility")
#Using the scale function to standardize the column values
Mva$Visibility.mi.<-scale(Mva$Visibility.mi.)
#Above plot for visibility shows that the data is not normal, and therefore would be an issue while 
#Statistical testing and hence the visibility attribute needs to be normalized

qqnorm(num[,c("Temperature.F.")],main="QQ Norm plot for Temperature")
#The qq plot for temperature shows that the data is almost perfectly normal

qqnorm(num[,c("Pressure.in.")],main="QQ Norm plot for Pressure")
#The above plot also has a skewed distribution and needs to be normalized

qqnorm(num[,c("Wind_Speed.mph.")],main="QQ Norm plot for Wind speed")
#above plot shows slight skewness in the wind speed normality with presence of outliers that must be eliminated in further analysis

qqnorm(num[,c("Precipitation.in.")],main="QQ Norm plot for Precipitation")
#Scaling the data 
Mva$Precipitation.in.<-scale(Mva$Precipitation.in.)
#qqnorm(num[,c("Precipitation.in.")],main="QQ Norm plot for Precipitation")
#Above plot shows presence of outliers and slight skewness



## CATEGORICAL VARIABLE EXPLORATION ##

library(ggplot2)

#Some examples of observations noted

#1. Sunrise_Sunset : whether accidents happened more at night or day
ggplot(Mva,aes(fill=Mva$Sunrise_Sunset,y=Mva$Severity,x=Mva$Temperature.F.))+
  geom_bar(stat='identity')
#Above graph shows that for the normally distributed temperature data,a high majority of the road accidents took place in the daytime
#Also, the peak was around 60 degrees F

#2. State :exploring which states had the most accidents and at what severity
ggplot(Mva, aes(x=Mva$State,y=Mva$Severity)) + 
  geom_histogram(stat='identity',binwidth=50)+
  theme(text = element_text(size=5),
        axis.text.x = element_text(angle=90, hjust=1)) 
#Here we observe that the most number of accidents took place in the state of California, Texas and Florida

#Ques 3. Side: To find which side of the road was more prone to the event of accident.
ggplot(Mva, aes(fill=Mva$Side, y=Mva$Severity, x=Mva$Wind_Chill.F.)) + 
  geom_bar(position="stack", stat="identity")
#We observe that most accidents took place on the right side of the road.

#Ques 4. To fond whether the roundabout presence or absence was significant for the accident.
ggplot(Mva, aes(fill=Mva$Roundabout, y=Mva$Severity, x=Mva$Wind_Speed.mph.)) + 
  geom_histogram(position="stack", stat="identity",xlim=c(0,1))
#Here we observe that mostly the roundabout was absent when the accident took place.


## PERFORMING HYPOTHESIS TESTS ## will be expanded more after further processing and normalizing of the data attributes

#t tests here determine whether the null hypothesis that the mean of the two samples is equal will be rejected or accepted
# Performing t-tests on various combinations of variables

t.test(Mva$Humidity...~Mva$Station)
# The p value comes out to be really small therefore the null hypothesis that the mean of both samples is equal is rejected

t.test(Mva$Severity~Mva$Side,data=Mva)
#Here again it is rejected, since p-value is extremely small


