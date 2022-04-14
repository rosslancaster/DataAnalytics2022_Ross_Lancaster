#Fitting Regression Trees

library(MASS)
install.packages('tree')
library(tree)
set.seed(1)
head(Boston)


train=sample(1:nrow(Boston),nrow(Boston)/2)
tree.boston=tree(medv~.,Boston,subset=train)

summary(tree.boston)
#Summary output indicates only three of the variables have been 
#used to construct the tree. In context of regression tree, the deviance
#is simply the sum of squared errors for the tree

#Regression tree
help(tree)
tree(formula=medv~.,data=Boston,subset=train)

#Plot the three
plot(tree.boston)
text(tree.boston,pretty=0)
#variable list "lstat" measure the percentage of the individuals with
#lower socioecnomic status
#Tree indicates that the lower values of lstat corresponds to a more expensive house

#Use cv.tree function to see whether pruning the tree will improve performance

help(cv.tree)
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,typ='b')

#Most complex tree is selected by cross-validation
#However if wish to prune the tree, we could use prune.tree

help(prune.tree)
prune.boston=prune.tree(tree.boston,best=5)
plot(prune.boston)
text(prune.boston,pretty=0)

#Keeping with cross validation results, we use the pruned tree
#to make predictions on the test set

yhat=predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,'medv']
plot(yhat,boston.test)

#add abline()
abline(0,1)
mean((yhat-boston.test)^2)

#The test set MSE associated with the regression tree is 35.28688
#take square root of MSE- indicates that this model leads to test 
#precitions that are wihtin ... of the true median home value for the suburb
#----------------------------------------------------------------------
#Random Forest Regression
#Like always, the randomForest package is not available for my version of R
#but I will type out the code for practice 
install.packages('randomForest')
library(randomForest)
set.seed(1)

bag.boston = randomForest(medv ~., data=Boston, subset = train, mtry=13, importance= TRUE)
bag.boston

# Argument mtry=13 indicates that all 13 predictors should be considered
# for each split of the tree-in other words, that bagging should be done.
# How well does this bagged model perform on the test set?

yhat.bag = predict(bag.boston ,newdata=Boston[-train ,])
plot(yhat.bag, boston.test)

# adding the abline()
abline(0,1)
mean((yhat.bag-boston.test)^2)

# The test set MSE associated with the bagged regression tree is 13.16,
#almost half that obtained using an optimally-pruned single tree.
#We could change the number of trees grown by randomForest() using the ntree
#argument:
bad.boston =
bag.boston=randomForest(medv~.,data=Boston,subset=train, mtry=13,ntree=25)
yhat.bag = predict(bag.boston ,newdata=Boston[-train ,])
mean((yhat.bag-boston.test)^2)

set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train, mtry=6,importance =TRUE)
yhat.rf = predict(rf.boston ,newdata=Boston[-train ,])
mean((yhat.rf-boston.test)^2)
# The test set MSE is 11.31;
# this indicates that random forests yielded an improvement over bagging in this case.
# Using the importance() function, we can view the importance of each variable.

importance (rf.boston)
# Two measures of variable importance are reported.
#The former is based upon the mean decrease of accuracy in predictions on
# the out of bag samples when a given variable is excluded from the model.
#The latter is a measure of the total decrease in node impurity that results
# from splits over that variable, averaged over all trees.
# In the case of regression trees, the node impurity is measured by the training RSS,
# and for classification trees by the deviance.
# Plots of these importance measures can be produced using the varImpPlot() function.
varImpPlot (rf.boston)