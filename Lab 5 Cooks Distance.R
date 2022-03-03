#Lab 5- Cook's Distance

#Use the mtcars dataset
mtcars
head(mtcars)
str(mtcars)

#Look at the variable distributions
attach(mtcars)
boxplot(mpg,cyl,disp,hp,drat,wt,qsec,vs,am,gear,carb)
#Variables with smaller values only
boxplot(mpg,cyl,drat,wt,qsec,vs,am,gear,carb)

summary(mtcars)

model1=lm(mpg~cyl+wt, data=mtcars)
model1

model2=lm(mpg~hp+qsec+carb,data=mtcars)
model2

help("cooks.distance")
plot(model1,pch=18,col='red',which=c(4))

#Find Cook's Distance for each observation
cooks.distance(model1)
CooksDistance=cooks.distance(model1)

#Round the distances to 5 decimal points
round(CooksDistance,5)

#Sort the distances in ascending order
sort(round(CooksDistance,5))

#Use cooks distance to identify outliers 
library(ISLR)

#Use baseball hitter's dataset
head(Hitters)
dim(Hitters)

summary(Hitters)

#Look at the distributions
attach(Hitters)
boxplot(AtBat,Hits,HmRun,Runs,RBI,Walks,Years,CAtBat,CHits,CHmRun,CRuns,CRBI,CWalks,PutOuts,Assists,Errors,Salary)

#Remove NA values
is.na(Hitters)
HittersData=na.omit(Hitters)

dim(HittersData)
glimpse(HittersData)
install.packages("glimpse")
#Glimpse does not seem to exist- I know it was mentioned in class but I don't remember how to fix it

head(HittersData)
summary(HittersData)

#Use mutlivariate regression model using all features to predict 
#salaries of baseball players

SalaryPredictModel1=lm(Salary~.,data=HittersData)
summary(SalaryPredictModel1)
# R-squared is 0.5461, Adjusted r-sqaured is 0.5106

#Use Cook's Distance
cooksD=cooks.distance(SalaryPredictModel1)
influential=cooksD[cooksD>(3*mean(cooksD,na.rm=TRUE))]
influential

#18 players have Cook's Distance greater than 3 times the mean
#Exclude these 18 players and rerun model

names_of_influential=names(influential)
names_of_influential

outliers=HittersData[names_of_influential,]
Hitters_Without_Outliers=HittersData %>% anti_join(outliers)

#Model without outliers
SalaryPredictModel2=lm(Salary~.,data=Hitters_Without_Outliers)
summary(SalaryPredictModel2)
#Multiple r squared 0.6721, adjusted r squared 0.6445