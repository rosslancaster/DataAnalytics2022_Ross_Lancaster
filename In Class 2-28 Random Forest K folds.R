#ISLR: Introduction to Statistical Learning wiht T (class textbook)
#Validation set example with Auto dataset

library(ISLR)
library(MASS)
library(boot)

set.seed(1)

??cv.glm

help('sample')
train=sample(392,196)
#We use teh subset option in the lm() function to fit linear regression using olny the observations corresponding to the training set

lm.fit=lm(mpg~horsepower,data=Auto,subset=train)

#Use predict() to estimate response for all 392 observations, use the mean
#function to calculate MSE of for the 196 observations in the validation set
#-train selects the observations not in the training set

attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
#Estimated MSE is 23.27

#Use poly() to estimate test error for quadratic and cubic regression

#Quadratic regression
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
#MSE is 18.71

#Cubic regression
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
#MSE is 18.79

#Choose different seed 
set.seed(2)
train=sample(392,196)
lm.fit=lm(mpg~horsepower,data=Auto,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
#Error is 25.63

lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
#Error is 19.98

lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
#Error is 20.39

#Not much different between performance of quadratic and cubic regressions

#-------------------------------------------------------
#K-fold cross validation

#Use cv.glm() to implement k-fold cross validation
#k=10

??cv.glm
set.seed(17)
help("rep")

cv.error.10=rep(0,10)
for(i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10
#Does not improve beyond improvement jump from linear to quadratic

#---------------------------------------------------------------------
#Random Forests

#Use car data from UCI ML repository (car.data and car.names)

install.packages("randomForest")
library(randomForest)
#My version of R does not support this package, I do not know how to fix this and will have to enquire in class but I will write out the code still for practice

set.seed(100)
train=sample(nrow(data1),0.7*nrow(data1),replace=FALSE)
TrainSet=data1[train,]
ValidSet=data[-train,]

summary(TrainSet)
summary(ValidSet)

help(randomForest)
#No documentation found, what a surprise

model1=randomForest(Condition~.,data=TrainSet,importance=TRUE)
model1

#Fine tune parameters
#Increase mtry to 6 from 2
#mtry=Number of variables randomly sampled as candidates at each split
#Default values different for classification(sqrt(p) where p is number of variables in x) and regression (p/3)

model2=randomForest(Condition~.,data=TrainSet,ntree=500,mtry=6, importance=TRUE)
model2

#First conduct prediction with training set and then prediction using Validation set

#Predict Train data
predTrain=predict(model2,TrainSet, type="class")
#Table to check the classification accuracy

#Predict Validation data
predValid=predict(model2,ValidSet,type="class")
table(predValid,ValidSet$Condition)

#Use importance function to check important variables
#Show the drop in mean accuracy for each of the variables to check the important variables

importance(model2)
varImpPlot(model2)

#Use for loop and check for different values of mtry
#Use for loop to identify right "mtry" for the model

a=c()
i=5
fpr (i in 3:8){
  model3=randomForest(Condition~.,data=TrainSet,ntree=500,mtry=i,importance=TRUE)
  predValid=predict(model3,ValidSet,type="class")
  a[i-2]=mean(predValid==ValidSet$Condition)
}
a
plot(3:8,a)

#Compare this model with decision tree 
library(rpart)
install.packages("caret")
library(caret)
library(e1071)

model_dt=train(Condition~.,data=TrainSet,method="rpart")
model_dt_1=predict(model_dt,data=TrainSet)
table(model_dt_1,TrainSet$Condition)
mean(model_dt_1==TrainSet$Condition)
table(model_dt_1,TrainSet$Condition)
mean(model_dt_1==TrainSet$Condition)

#Check Validation data
model_dt_vs=predict(model_dt,newdata=ValidSet)
table(model_dt-Vs,ValidSet$Condition)
mean(model_dt_vs==validSet$Condition)
