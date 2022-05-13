setwd("/Users/hlmed/Desktop/ACST3061A")


#We will first obtain our data from iLearn and upload it to Rstudio and this can be done by using read.csv() command, 
#then we will use the attach function to make data frame variable to global variable thus we can obtain the data just by entering the name of the variable we desire to lookup. 
#We will use summary() command to create a table which will generate a brief summary of the data in crania file. Also through this data, 
#we can look at the Min, Max and Mean value. Through these we can have a basic understanding if the data are skewed thus we can perform a log transformation if required. 



crania = read.csv("crania.csv", header = T)

attach(crania)
names(crania)

summary(crania)

View(crania)


#Check if we require any log transformation

#After looking at the summary data, which might not be enough to determine if we require any data transformation work. 
#Now we will create a histogram for each variable that we are interested. 
#Histogram are very useful visual tools to spot any skewed data.

par(mfrow=c(1,2))

hist(AbsoluteLatitude)
hist(CranialCapacity)
hist(FMarea_intercondyle)
hist(MeanOrbitalVolume)
hist(Minimum_Illuminance)
hist(Minimum_Temperature_celsius)

LNAbslat = hist(log(AbsoluteLatitude))
LNTemp = hist(log(Minimum_Temperature_celsius))

#Convert population and gender as factor

#As we can see in the histogram, the data distributed around the centre except for Minimum temperature and absolute latitude, 
#some data in these 2 concentrated on the right as it is evident in the graph above. However in general there is no log transformation needed for these data. 
#After looking at the data, we can spot that there are 2 categorical covariate and they are gender and population. 
#And linear regression can totally include categorical data as part of the variable. 
#However, we will have to first convert these categorical data as a factor first otherwise R cannot identify these variables. 


crania$Gender = as.factor(crania$Gender)
crania$Population = as.factor(crania$Population)

table(crania$Gender)
table(crania$Population)

#Data Analysis

library(psych)

#Use pairs and pairs.panels command to create a matrix graph for better visualisation to interpret the data. 
#And seeing the relationship between each variables. For the pairs.panels command a psych package is needed and 
#we will download the package using install.packages(psych) command and use library(psych) to open the package. 
#In the pairs panel graph, it shows the correlation, trendline and the concentrated data region between each covariates, 
#there appears to be a strong positive correlation between mean orbital volume with cranial capacity and absolute latitude, and a strong negative correlation with minimum illuminance. 


crania.1 = data.frame(AbsoluteLatitude, CranialCapacity, FMarea_intercondyle,
             MeanOrbitalVolume, Minimum_Illuminance, Minimum_Temperature_celsius)

crania.2 = data.frame(AbsoluteLatitude, CranialCapacity, FMarea_intercondyle,
                      MeanOrbitalVolume, Minimum_Illuminance, Minimum_Temperature_celsius,
                      crania$Gender, crania$Population)

pairs(crania.2, cex.labels =1)

pairs.panels(crania.1, cex.cor = 1)


#Linear model of Mean Orbital Volume with other variables

#Now we will perform our model fitting, model.MOV is the multiple linear model with response variable Mean orbital volume against the other 7 covariates. 
#The code and R console output is provided below. 

model.MOV.CC = lm(MeanOrbitalVolume~ CranialCapacity, data = crania.2)
plot(CranialCapacity, MeanOrbitalVolume)
abline(model.MOV.CC)
summary(model.MOV.CC)
plot(model.MOV.CC)

model.MOV.MI = lm(MeanOrbitalVolume~Minimum_Illuminance, data = crania.2)
plot(Minimum_Illuminance, MeanOrbitalVolume)
abline(model.MOV.MI)
summary((model.MOV.MI))

model.MOV.MT = lm(MeanOrbitalVolume~Minimum_Temperature_celsius, data = crania.2)
plot(Minimum_Temperature_celsius, MeanOrbitalVolume)
abline(model.MOV.MT)
summary(model.MOV.MT)

model.MOV.FM = lm(MeanOrbitalVolume~FMarea_intercondyle, data = crania.2)
plot(FMarea_intercondyle, MeanOrbitalVolume)
abline(model.MOV.FM)
summary(model.MOV.FM)

model.MOV.AL = lm(MeanOrbitalVolume~AbsoluteLatitude, data = crania.2)
plot(AbsoluteLatitude, MeanOrbitalVolume)
abline(model.MOV.AL)
summary(model.MOV.AL)
plot(model.MOV.AL)

model.MOV.Gender = lm(MeanOrbitalVolume~crania.2$crania.Gender, data = crania.2)
summary(model.MOV.Gender)
plot(model.MOV.Gender)

model.MOV.Population = lm(MeanOrbitalVolume~crania.2$crania.Population, data = crania.2)
summary(model.MOV.Population)


#Use forward selection to refine the linear model

#For the forward selection method, we will start from the base case, which is the response covariate only test against one explanatory covariate (simple linear regression). 
#And we will consider the p value of the ß and it associated p value. And we will choose the covariate regression coefficient with the lowest p value. 
#Then we will continue the progress for 2 covariates and more until there is the ß of the coefficient is insignificant. The method can be obtained from lecture note. 
#This method can be pefromed manually on R or we can use the package olsrr which is a linear model, covariate selection algorithm package that will perform forward selection algorithm automatically. 
#And for this investigation we will the latter method using the statsictical package olsrr, because of the limited page limit for this assignment. 
#And using the olsrr package require less coding and generate a comprehensive detail to report the forwards step selection and their p-value. 
#The code and result are provided below:

step1.1 = lm(MeanOrbitalVolume~ CranialCapacity, data = crania.2)
summary(step1.1)

step1.2 = lm(MeanOrbitalVolume~Minimum_Illuminance, data = crania.2)
summary(step1.2)

step1.3 = lm(MeanOrbitalVolume~Minimum_Temperature_celsius, data = crania.2)
summary(step1.3)

step1.4 = lm(MeanOrbitalVolume~FMarea_intercondyle, data = crania.2)
summary(step1.4)

step1.5 = lm(MeanOrbitalVolume~AbsoluteLatitude, data = crania.2)
summary(step1.5)

step1.6 = lm(MeanOrbitalVolume~crania.2$crania.Gender, data = crania.2)
summary(step1.6)

step1.7 = lm(MeanOrbitalVolume~crania.2$crania.Population, data = crania.2)
summary(step1.7)

#During step 1,Absolute latitude obtain the lowest p-value, thus we add this covariate in the linear model for the next step
#Now we continuw with step 2

step2.1 = lm(MeanOrbitalVolume~AbsoluteLatitude+CranialCapacity, data = crania.2)
summary(step2.1)

step2.2 = lm(MeanOrbitalVolume~AbsoluteLatitude+Minimum_Temperature_celsius, data = crania.2)
summary(step2.2)

step2.3 = lm(MeanOrbitalVolume~AbsoluteLatitude+FMarea_intercondyle, data = crania.2)
summary(step2.3)

step2.4 = lm(MeanOrbitalVolume~Minimum_Illuminance+AbsoluteLatitude, data = crania.2)
summary(step2.4)

step2.5 = lm(MeanOrbitalVolume~AbsoluteLatitude+crania.2$crania.Gender, data = crania.2)
summary(step2.5)

step2.6 = lm(MeanOrbitalVolume~AbsoluteLatitude+crania.2$crania.Population, data = crania.2)
summary(step2.6)

#During step 2, the linear model of Minimum illuminance and Gender have the lowest p-value
#Now we continue with step 3

step3.1 = lm(MeanOrbitalVolume~AbsoluteLatitude+crania.2$crania.Gender+CranialCapacity, data = crania.2)
summary(step3.1)

step3.2 = lm(MeanOrbitalVolume~AbsoluteLatitude+crania.2$crania.Gender+Minimum_Temperature_celsius, data = crania.2)
summary(step3.2)

step3.3 = lm(MeanOrbitalVolume~AbsoluteLatitude+crania.2$crania.Gender+FMarea_intercondyle, data = crania.2)
summary(step3.3)

step3.4 = lm(MeanOrbitalVolume~Minimum_Illuminance+crania.2$crania.Gender+AbsoluteLatitude, data = crania.2)
summary(step3.4)

step3.5 = lm(MeanOrbitalVolume~AbsoluteLatitude+crania.2$crania.Gender+crania.2$crania.Population, data = crania.2)
summary(step3.5)



#Building linear model with response variable is Mean Orbital Volume

library(olsrr)

model.MOV = lm(MeanOrbitalVolume~Minimum_Illuminance+crania.2$crania.Gender+CranialCapacity+
                 Minimum_Temperature_celsius+FMarea_intercondyle+
                 AbsoluteLatitude+crania.2$crania.Population, data = crania.2)
ols_step_forward_p(model.MOV, details = T)
summary(model.MOV)
anova(model.MOV)

plot(step3.1)

anova(step3.1)

model.MOV.final = lm(MeanOrbitalVolume~AbsoluteLatitude+crania.2$crania.Gender+CranialCapacity, data = crania.2)

par(mfrow = c(2,2))
plot(AbsoluteLatitude, resid(model.MOV.final), main = 'Residual against Absolute Latitude')
plot(CranialCapacity, resid(model.MOV.final), main = 'Residual against Cranial Capacity')
plot(crania.2$crania.Gender, resid(model.MOV.final), main = 'Residual against Gender')
plot(fitted(model.MOV.final), resid(model.MOV.final), main = 'Residual against fitted data')


