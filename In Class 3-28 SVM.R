#Support Vector Machines

data(iris)
head(iris)

#I know the structure of this dataset

#load necessary packages
library(ggplot2)
library(e1071)

#Separate Species based on color and plot with petal.length vs. petal.width
#can see separation of Setosa- overlappings in Versicolor and Virginica
#now can use qplot() function, X=petal.length, y=petal.width- separate by color
qplot(Petal.Length,Petal.Width,data=iris,color=Species)

#Use SVM function
#Name first model svmmodel1
help(svm)
svmmodel1=svm(Species~.,data=iris)

#use summary to see the results
summary(svmmodel1)

#Plot the model
plot(svmmodel1,data=iris,Petal.Width~Petal.Length,slice=list(Sepal.Width=3,Sepal.Length=4))

#Predict using the model
pred1=predict(svmmodel1,iris)
table1=table(Predicted=pred1,Actual=iris$Species)
table1

#Calculate the accuracy of the model
model1accuracyrate=sum(diag(table1))/sum(table1)
model1accuracyrate

#Calculate missclassification rate of the model
modelmissclassrate=1-model1accuracyrate
modelmissclassrate
#----------------------------------------------------------------------------------------------
#Model 2

#kernel=linear
svmmodel2=svm(Species~.,data=iris,kernel="linear")

summary(svmmodel2)

plot(svmmodel2,data=iris,Petal.Width~Petal.Length,slice=list(Sepal.Width=3,Sepal.Length=4))

pred2=predict(svmmodel2,iris)
table2=table(Predicted=pred2,Acutal=iris$Species)
table2

model2accuracyrate=sum(diag(table2))/sum(table2)
model2accuracyrate

model2missclassrate=1-model2accuracyrate
model2missclassrate

#model 2 is less accurate than model 1
#----------------------------------------------------------------------------------------------
#Model 3

#kernel=polynonmial
svmmodel3=svm(Species~.,data=iris,kernel="polynomial")

summary(svmmodel3)

plot(svmmodel3,data=iris,Petal.Width~Petal.Length,slice=list(Sepal.Width=3,Sepal.Length=4))

pred3=predict(svmmodel3,iris)
table3=table(Predicted=pred3,Acutal=iris$Species)
table3

model3accuracyrate=sum(diag(table3))/sum(table3)
model3accuracyrate

model3missclassrate=1-model3accuracyrate
model3missclassrate

#model 3 is even less accurate than model 2- tsk tsk model 3