#Lab 7

#Trees for the Titanic dataset is already submitted on Github

#SVM
library(e1071)
set.seed(1)
#use the svm function to fit the supprt vector classifier for a given value
# of the cost parameter
#Demonstrate use of function on 2-D example so we can plot the decision boundary

#generate observations which belong to two classes
x=matrix(rnorm(20*2),ncol=2)
y=c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1
x
y

#check wheter the classes are linearly separable
plot(x,col=(3-y))
#they are not linearly separable

#In order for svm function to perform classification must encode the response
#as a factor variable
#Create new data frame with response coded as a factor
dat=data.frame(x=x,y=as.factor(y))
help(svm)
svmfit=svm(y~.,data=dat,kernel='linear',cost=10,scale=FALSE)

#Scale=Flase tells svm function not to scale each feature to have mean 
#of zero or standard deviation one

#Plot the support vector classifier
plot(svmfit,dat)
#Two arguments to plot svm function are the output of the call to svm as 
#well as the data used in the call to svm
#Region of feature space that will be assigned to the -1 class is shown in light blue
#and the region that will be assigned to the +1 class is shown in purple

svmfit$index
#1,2,5,14,16,17-must have used the original seed before you changed it

summary(svmfit)
#Seven support vectors, four in one class, three in the other

#Use a smaller cost, cost=0.1
svmfit=svm(y~.,data=dat,kernel='linear',cost=0.1,scale=FALSE)
plot(svmfit,dat)
svmfit$index
#1,2,3,4,5,7,9,10,12,13,14,15,16,17,18,20- more support vectors

#Cost smaller, margin wider, more support vectors

#e1071 has a built in tune function to perform cross validation
#By default tune() performs ten-fold cross validation on a set of models of interest
#Need to pass relevant info about the set of models under consideration

#Following commands indicate that we want to compare SVms with a linear kernel
#using range of values of the cost parameter

set.seed(1)
tune.out=tune(svm,y~.,data=dat,kernel='linear',ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
#Easily access the cross validation errors for each of these models using summary
summary(tune.out)
#0.1 had the lowest cross-validation error rate
#tune function stores best model obtained

bestmod=tune.out$best.model
summary(bestmod)

#predict function can be used to predict the class label on a set of observations
#at any given value of the cost parameter

#Generate test data
xtest=matrix(rnorm(20*2), ncol=2)
ytest=sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,] + 1
testdat=data.frame(x=xtest, y=as.factor(ytest))
#Predict the class label of the test observations
#use best model obtained through cross validation in order to make prediction

ypred=predict(bestmod,testdat)
table(predict=ypred,truth=testdat$y)

#What if we used cost=0.01 instead
svmfit=svm(y~.,data=dat,kernel='linear',cost=0.01,scale=FALSE)
ypred=predict(svmfit,testdat)
table(predict=ypred,truth=testdat$y)

#Consider situation in which the two classes are linearly separable
#Find separating hyperplane using svm function
#First separate two classes in simulated data so that they are linearly separable
x[y==1,]=x[y==1,]+0.5
plot(x, col=(y+5)/2, pch=19)

#Observations are just barely linearly separable
#Fit the support vecto classifier and plot hte resulting hyperplane
#using a very large value of cost so that no observations are misclassified
dat=data.frame(x=x,y=as.factor(y))
svmfit=svm(y~.,data=dat,kernel='linear',cost=1e5)
summary(svmfit)
plot(svmfit,dat)

#no training errors were made and only 3 support vectors
#Margin is very narrow- seem like it will perform poorly on test data
#Try smaller cost

svmfit=svm(y~.,data=dat,kernel='linear',cost=1)
summary(svmfit)
plot(svmfit,dat)
#Likely this model will perform better on test data than cost=1e5

#Khan data set- which consists of a number of tissue samples
#corresponding to four distinct types of small round blue cell tumors
#For each tissue sample, gene expression measurements are available

#Data set contains training data, xtrain and ytrain, and testing data,
#xtest and ytest
library(e1071)
library(ISLR)
names(Khan)

dim(Khan$xtrain)
dim(Khan$xtest)

length(Khan$ytrain )
length(Khan$ytest )
table(Khan$ytrain )
table(Khan$ytest )

#Will use support vector approach to predict cancer subtype using gene
#expression measurements
#Very large number of features relative to the number of observations
#Suggests we should use a linear kernel, additional flexibility will
# result from using a polynomial or radial kernel is unnecessary

dat=data.frame(x=Khan$xtrain , y = as.factor(Khan$ytrain ))
out=svm(y ~., data=dat, kernel="linear",cost=10)
summary(out)

#No training errors, not suprising because large number of variables relative
#to number of observations implies it is easy to find hyperplane that 
#fully separates the classes

#More interested in performance on test observations

dat.te=data.frame(x=Khan$xtest , y = as.factor(Khan$ytest ))
pred.te=predict(out, newdata=dat.te)
table(pred.te, dat.te$y)
# We see that using cost=10 yields two test set errors on this data.
#---------------------------------------------------------------------
#DPYR package
# Dplyr for Data Manipulating
# Tidyr for Data Cleaning
# Pipe Operator    %>% 

# Dplyr 
install.packages('dplyr')
# We will be using the NYC 2013 flights data during this examples, first install the package
install.packages('nycflights13')

library(dplyr)
# You will see: **The following objects are masked from ‘package:stats’: filter, lag **
# This is becasue dplyr will have it's own filter() function and lag() function which is different from the usual filter function that come with the Base R package.
library(nycflights13)
head(flights)
summary(flights)


# filter() function in dplyr allows us to select a subset of rows in a dataframe.
# it allows us to filter by conditions
filter(flights,month == 03, day == 28, carrier =='AA')
head(filter(flights, month == 03, day == 28, carrier == 'AA'))
# instead of using the dplyr, we can use the [ ] notation, it is long and messy :(
head(flights[flights$month == 03 & flights$day == 28 & flights$carrier == 'AA' , ]) # here I have to keep calling the dataframe name, and use the logical operators with '&' and combine them.

# slice() in dplyr
# slice() function  allows us to select rows by the position
slice(flights, 1:15) # selecting first 15 rows

# arrange() in dplyr
# arrange() function works similar to filter() function except that instead of filtering or selcting rows, it reorder the rows
arrange(flights,year,month,day, arr_time)
head(arrange(flights,year,month,day,arr_time))
# if I want to use the descending time instead of accending time, 
head(arrange(flights,year,month,day, desc(arr_time)))

# select() in dplyr
select(flights,carrier)
head(select(flights,carrier))
# We can add aditional columns easily 
head(select(flights, carrier, arr_time))
head(select(flights, carrier, arr_time, day))
head(rename(flights, airline.carrier = carrier))

# distinct() in dplyr
# distinct() function in dplyr helps us to select the distinct or unique values in a column.
distinct(select(flights, carrier))

# mutate() in dplyr
# in additing to selecting sets of existing columns in the dataframe, sometimes 
# we need to add new columns that are functions of existing columns in the dataframe.
# we can use the mutate() function to do that.
head(mutate(flights, MyNewColumn = arr_delay - dep_delay))
# If you only want to see the new column instead of calling the mutate, you can 
# use the transmute() fuction.
# The difference between the mutate() and transmute() is that mutate() function returns
# the entire dataframe along with the new column and the transmute() shows only the new column.
head(transmute(flights, MyNewColumn = arr_delay - dep_delay))

# summarise() in dplyr
# The summarize() allows us to summarize the data frame into a single row using another aggrigate function
summarise(flights, avg_air_time = mean(air_time, na.rm = TRUE)) # average airtime
summarise(flights, TotalFlightTime = sum(air_time, na.rm = TRUE)) # Total Flight Time

# sample_n() in dplyr
# sample_n() function allows us to pick random number of rows that we wish to choose:
sample_n(flights, 15) # random 15 rows. 
sample_n(flights, 71) # random 71 rows. 

# sample_frac() in dplyr
# if you wan to pick a percentage of rows, sample_frac() function allow us to do that,
# you need to assign the fraction, example: 30% = 0.3, similaly 10% = 0.1
sample_frac(flights,0.1) # sample with a 10% of rows from the total number of rows 
sample_frac(flights, 0.3) # sample with a 30% of rows from the total number of rows 
sample_n(flights, 30)
sample_frac(flights, 0.5)
# dbl stands for doubles, or real numbers.
# dttm stands for date-times (a date + a time).

# Pipe operator:  %>%
library(dplyr)
df_mtcars <- mtcars

head(df_mtcars)

# nesting 
filter(df_mtcars, mpg > 20) # filter mpg > 20
# we want to get 10 samples of that
sample_n(filter(df_mtcars, mpg > 20), 10)
# now we want to arrange them in the descending order based on the mpg
arrange( sample_n(filter(df_mtcars, mpg >20), 10) ,desc(mpg))
# we can assign this result to a variable called results_mpg
results_mpg <- arrange( sample_n(filter(df_mtcars, mpg >20), 10) ,desc(mpg))
results_mpg

# You can do the above using the Pipe Operator %>%
# dataFrame %>% op1 %>% op2 <$op3

