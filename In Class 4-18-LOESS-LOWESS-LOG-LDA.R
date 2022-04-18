#Local Regression
help(loess)

data(economics, package="ggplot2") # load data
economics$index <- 1:nrow(economics) # create index variable
economics = economics[1:80, ] # retail 80rows for better graphical understanding
loessMod10 = loess(uempmed ~ index, data=economics, span=0.10) # 10% smoothing span
loessMod25 = loess(uempmed ~ index, data=economics, span=0.25) # 25% smoothing span
loessMod50 = loess(uempmed ~ index, data=economics, span=0.50) # 50% smoothing span

# Predict Loess
smoothed10 = predict(loessMod10)
smoothed25 = predict(loessMod25)
smoothed50 = predict(loessMod50)
# From above plot, you would notice that as the span increases, the smoothing of the curve also increases.

# Plot it
plot(economics$uempmed, x=economics$date, type="l", main="Loess Smoothing and Prediction",
     xlab="Date", ylab="Unemployment (Median)")
lines(smoothed10, x=economics$date, col="red")
lines(smoothed25, x=economics$date, col="green")
lines(smoothed50, x=economics$date, col="blue")

#Use 60 and 70% span
loessMod60 = loess(uempmed ~ index, data=economics, span=0.60) # 60% smoothing span
loessMod70 = loess(uempmed ~ index, data=economics, span=0.60) # 70% smoothing span
smoothed60 = predict(loessMod60)
smoothed70 = predict(loessMod70)
lines(smoothed60, x=economics$date, col="purple")
lines(smoothed70, x=economics$date, col="orange")
#Gets even smoother but also seems to not model the data as well

# Fitting a curve to the data

# LOWESS example using the Cars dataset
data("cars")

str(cars) #50 observation and 2 variables
# plot speed Vs distance
plot(speed ~ dist, data = cars)
#positive relationship between these two variables

help(lowess)

boxplot(cars)
plot(cars)
#lowess() function
lowess(cars$speed ~ cars$dist)
#line()function to draw the lines
lines(lowess(cars$speed ~ cars$dist, f=2/3), col="blue")
# here the f value is the the smoother span. f= 2/3 = 0.666
# the default value for smoother span is 0.666 

lines(lowess(cars$speed ~ cars$dist, f=2/3), col="blue")

#This gives the proportion of points in the plot which influence the smooth at each
#value.
# Larger values give more smoothness.
# Change the "f" value and observe the shape of the line.
lines(lowess(cars$speed ~ cars$dist, f=0.75), col="gray") # f = 0.75
lines(lowess(cars$speed ~ cars$dist, f=0.8), col="red") # f = 0.8
lines(lowess(cars$speed ~ cars$dist, f=0.9), col="green") # f = 0.9
lines(lowess(cars$speed ~ cars$dist, f=0.1), col= 5) # f = 0.1
lines(lowess(cars$speed ~ cars$dist, f=0.01), col= 6) # f = 0.01
# Observe that, when we try to have a very lower values for "f", in this example, it will
#try to overfit points.

#---------------------------------------------------------------------------
# Logistic Regression Example

library(ISLR)
data("Smarket")
head(Smarket)
# data set consists ofpercentage returns for the S&P 500 stock index 
names(Smarket)
# For each date, we have recorded the percentage returns for each of the five previous trading days, "Lag1" through "Lag5".
# We have also recorded "Volume" (the number of shares traded on the previous day)
# "Today" is the percentage return on the date
# "Direction" (whether the market was Up or Down on this date)-target label

dim(Smarket)
summary(Smarket)
cor(Smarket)
#The first command below gives an error message because the "Direction" variable is qualitative.
cor(Smarket[,-9]) # omit target label
# the correlations between the lag variables and today's
# returns are close to zero.  The only substantial correlation is between Year and Volume
attach(Smarket)
plot(Volume)

#Fit a Logistic Regression model in order to predict "Direction"
# using "Lag1" through "Lag5" and "Volume".
# The glm() function fits generalized glm() linear models,

help("glm")
# glm() function is similar to that of lm(), except that we must pass in
# the argument family=binomial in order to run a logistic regressionrather than some other type of generalized linear model
glm.fit.model1 = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = binomial)
summary(glm.fit.model1)

# smallest p-value associated with Lag1.
# The negative coefficient for this predictor suggests that if the market
# had a positive return yesterday, then it is less likely to go up .
# At a value of 0.145, the p-value is still relatively large,
# and so there is no clear evidence of a real association between "Lag1" and "Direction".

# The The type="response" option tells R to output probabilities of the form P(Y = 1|X), as opposed
# to other information such as the logit.
# If no data set is supplied to the predict() function, then the probabilities are computed for the training
# data that was used to fit the logistic regression model.
# Here we have printed only the first ten probabilities. We know that these values correspond to
# the probability of the market going up, rather than down, because the
# contrasts() function indicates that R has created a dummy variable with a 1 for Up.
glm.probs = predict(glm.fit.model1, type="response")
glm.probs[1:10]
contrasts(Direction)

# In order to make a prediction as to whether the market will go up or
# down on a particular day, we must convert these predicted probabilities
# into class labels, Up or Down.
# The following two commands create a vector of class predictions based on
# whether the predicted probability of a market increase is greater than or less than 0.5
help("rep")
glm.pred = rep("Down", 1250) # this command creates a vector of 1,250 "Down" elements
glm.pred[glm.probs > 0.5] = "Up"
# >0.5=up

#Make a table
table(glm.pred, Direction)

(507+145)/1250
#0.5216

mean(glm.pred == Direction)
# 0.5216

# Next, in order to implement this strategy, we will first create a vector corresponding
# to the observations from 2001 through 2004. We will then use this vector
# to create a held out data set of observations from 2005.
train <- (Year <2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005 <- Direction[!train]


glm.fit.model2 <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 +Volume, data=Smarket,
                      family = binomial, subset = train)
glm.prob2 = predict(glm.fit.model2, Smarket.2005, type="response")

glm.pred2 <- rep("Down", 252)
glm.pred2[glm.prob2 > 0.5] = "Up"
table(glm.pred2, Direction.2005)

glm.fit.model3 <- glm(Direction ~ Lag1 + Lag2, data = Smarket, family = binomial,
                      subset = train)
glm.probs3 <- predict(glm.fit.model3, Smarket.2005, type = "response")
glm.pred3 <- rep("Down", 252)
glm.pred3[glm.probs3 > 0.5] = "Up"
table(glm.pred3, Direction.2005)
mean(glm.pred3 == Direction.2005)
# 0.5595238 approximately 0.56

#----------------------------------------------------
#LDA
library(MASS)
names(iris)
dim(iris) #  150 rows and 5 columns
head(iris)

#set the seed
set.seed(555)
Train = sample(1:nrow(iris), nrow(iris)/2)
iris_Train = iris[Train,] # Traning dataset
irist_Test = iris[-Train,] # Testing dataset

help(IDA)
fit1 = lda(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris_Train)

#Make predictions
predict1 <- predict(fit1, iris_Train)
predict1_class <- predict1$class

#Make a table
table1 <- table(predict1_class, iris_Train$Species)
table1
#It's perfect

sum(diag(table1))/sum(table1)
