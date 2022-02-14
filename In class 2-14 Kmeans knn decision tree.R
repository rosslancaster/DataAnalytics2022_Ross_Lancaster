#Classification
#install and load the necessary packages
install.packages("rpart")
library(rpart)
install.packages('rpart.plot')
library(rpart.plot)

#Look at the iris dataset
iris
dim(iris)

#Create a sample 
s_iris=sample(150,100)
s_iris

#Create training and testing datasets
iris_train=iris[s_iris,]
iris_test=iris[-s_iris,]
dim(iris_train)
dim(iris_test)

#Create a decision tree model 
decisiontreemodel=rpart(Species~.,iris_train,method='class')
#decisiontreemodel=rpart(Species~ iris$Sepal.Length+iris$Sepal.Width+iris$Petal.Length,+iris$Petal.Width)
# ~. means run all remaining columns
decisiontreemodel

#Visualize the decision tree model 
rpart.plot(decisiontreemodel)

#--------------------
#KNN
#read abalone dataset from UCI dataset
abalone=read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"), header=FALSE,sep=',')
abalone                 

#Column names
colnames(abalone)=c('sex','length','diameter','height','whole_weight','shucked_weight','viscera_Weight','shell_weight','rings')                 

#summarize 
summary(abalone)

#Structure of the abalone dataset
str(abalone)

#Rings range from 1-29, break rings into levels
#Less that 8=young, 8-11=adult, greater than 11=old
abalone$rings=as.numeric(abalone$rings)
abalone$rings=cut(abalone$rings,br=c(-1,8,11,35),labels=c('young','adult','old'))
abalone$ring=as.factor(abalone$rings)
summary(abalone$rings)

#Need to remove sex variable because KNN requires only numeric variables
aba=abalone
aba$sex=NULL

#Normalize the data using min max normalization
normalize=function(x){
  return((x-min(x))/(max(x)-min(x)))
}
aba[1:7]=as.data.frame(lapply(aba[1:7],normalize))
summary(aba$shucked_weight)
#Variables now have minimums of 0 and maximums of 1

#Split the data into training and testing sets
ind=sample(2,nrow(aba),replace=TRUE,prob=c(0.7,0.3))
KNNtrain=aba[ind==1,]
KNNtest=aba[ind==2,]
sqrt(2918)
#Make K equal to the square root of the number of observations (2918)
#Want to round to an odd number so that ties can be broken

#install class library 
install.packages('class')
library(class)

help(knn)
KNNpred=knn(train=KNNtrain[1:7],test=KNNtest[1:7],cl=KNNtrain$rings,k=55)
KNNpred
table(KNNpred)
#-------------------------------
#Kmeans
library(ggplot2)

#Use iris dataset
head(iris)
str(iris)
#Three species, setosa, versicolor, and virginica
summary(iris)

##Plot Sepal.Length vs Sepal.Width
ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width,col=Species))+geom_point()

##Plot Petal.Length vs Petal.Width
ggplot(iris,aes(x=Petal.Length,y=Petal.Width,col=Species))+geom_point()
##Separation is see easier with Petal Length vs Petal Width

set.seed(300)
kmax=12
#tot.withinss=Total within-cluster sum of square
#iter.max-maximum number of iterations
#nstart= if centers is a number, how many random sets should be chosen
wss=sapply(1:kmax,function(k){kmeans(iris[,3:4],k,nstart=20,iter.max=20)$tot.withiness})
plot(1:kmax,wss,type="b",xlab="Number of clusters(k)",ylab="Within cluster sum of squares")
#The plot does not work and I do not know why 

icluster=kmeans(iris[3:4],3,nstart=20)
table(icluster$cluster,iris$Species)
