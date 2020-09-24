# MVA Project
MultiVariate Analysis Fall 2020

## US Accidents, A countrywide traffic accident dataset(2016-2020) KAGGLE
MVA Project repository 

### Team Members
1. Prateek Singh Thind
    - MBA student with specializations in Supply Chain and Analytics & Information Management
    - Work ex: software developer and project leader with Infosys (Pune, India)
2. Mihika Gupta
    - Pursuing Master's of Information Technology and Analytics

### Data Description

This is a countrywide car accident dataset, which covers 49 states of the USA. The accident data are collected from February 2016 to June 2020, using two APIs that provide streaming traffic incident (or event) data. These APIs broadcast traffic data captured by a variety of entities, such as the US and state departments of transportation, law enforcement agencies, traffic cameras, and traffic sensors within the road-networks. Currently, there are about 3.5 million accident records in this dataset. [Click here](https://www.kaggle.com/sobhanmoosavi/us-accidents) to learn more about this dataset. 

### Problem Statement
Using MultiVariate Analysis Techniques to:
* Determine variables that contributed most to the US Road accidents.
* Predict Cross-Dependency of various data Attributes contributing to Road Accidents.
* Find and Predict patterns, if any, involving the parameters.
* Apply MVA Techniques to find insights into the data and Predict Future possibilities of Road accidents using historical data analysis. 

### MetaData
The dataset has approximately 3.5 million instances,and 49 attributes. Many attributes have missing or null values which will be trimmed consequently as we progress further with the analysis.

### Data Dictionary(Major Selected Attributes)

Index|Attribute Name|Description
-----|--------------|-----------
1|ID|Unique Accident ID
2|Severity|Shows severity on a scale of 1-4, 1 indicates least impact on traffic(short delay)
3|Start Time|shows the accident start time
4|End time|Shows the accident end time
5|Distance|The length of road affected by the accident
6|City|City of accident occurence
7|Temperature|shows temperature in deg F
8|Wind_Chill|Shows the wind chill in deg F
9|Humidity%|Shows the humidity in percentage 
10|Pressure(in)|Shows the air pressure in inches
11|Visibility|Shows visibility in Miles
12|Wind Direction|Shows wind direction
13|Wind Speed|Shows wind speed in mph(miles per hour)
14|Precipitation(in)|Shows precipitation in inches
15|Weather Conditions|Shows the weather conditions(rain,snow,fog,thunderstorm,etc)
16|Bump|Indicates presence of speed bump in a nearby area
17|Crossing|indicates a crossing in the nearby area
18|Sunrise_Sunset|Shows the period of the day(i.e. day or night)


# SPAP ANALYSIS (Assignment 2)

### Inspiration
US-Accidents can be used for numerous applications such as real-time car accident prediction, studying car accidents hotspot locations, casualty analysis and extracting cause and effect rules to predict car accidents, and studying the impact of precipitation or other environmental stimuli on accident occurrence. The most recent release of the dataset can also be useful to study the impact of COVID-19 on traffic behavior and accidents.



# SMART GOAL
The KPI (Key Performance Indicator) for our project is the "SEVERITY" attribute of the US Accidents Dataset.Our approach towards the end goal would be tri-fold in this project.
The relationships of three categories of variables with the KPI would be analysed and contribution of these attributes would be evaluated towards prediction of a potential road accident.They are as follows:
### Analyzing Cause and Effect Relationship between:

1. **Severity vs Weather Conditions**
Attributes like Temp, Humidity, Wind chill, Precipitation, Pressure, Visibility, Wind Direction etc will be used.
2. **Severity vs Structural Specifications of the Accident location**
Attributes like Road stops, roundabouts, traffic calming, signals and turning points would be analyzed.
3. **Severity vs Geographical location and time of event of accident**
Further attributes like City, Country, County, time, area would be analyzed.

# Dependent variables
Evaluation of each of the three goals (Weather, infrastructure and location) invloves three sets of dependent variables as mentioned in detail above.
The common and most important Dependent variable would be "Severity", that directly indicates the level of accident in all three goal categories.

# Specific Questions
Q1. If weather is a dependent variable for road accidents prediction, how does increasing or decreasing the weather parameter affect the severity?
Q2. If Infrastructure is a dependent variable, how does the presence or absence of road signals, bumps, turning points affect the event of an accident occurance and its severity?
Q3. If Location of accident is a dependent variable, how does the area, city , country, time of accident affect the severity?

# Independent Variables
Further each of the Depenedent variable ( Weather , Infrastructure and Location) can be atomized into their own independently contributing variables
### Weather 
Weather has independent attributes like: Temp, Humidity, Wind chill, Precipitation, Pressure, Visibility, Wind Direction
### Infrastructure
This has independent attributes like:Road stops, roundabouts, traffic calming, signals and turning points
### Geographical Location and Time
This has independent variables like:City, Country, County, time, area

# Specific analyses and graphs
For all three dependent variables, various plots and visualizations will be illustrated:
1. Correlation plots between all independent variables, to find any interdependency or patterns
2. Dependent variables vs severity
3. Independent variables vs severity
4. Quantitative and numerical attributes can be processed using statistical/ MVA models and graphs representing probability or % contribution to the accident will be plotted.
5. Categorical variables (Present/Absent or Day/Night) can be used to determine which category was more prevelant when an accident occured in general.


Further progress would be updated as we move ahead with the project and explore the dataset in detail!
