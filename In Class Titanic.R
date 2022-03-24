#Titanic Data Exercise

data(Titanic)
View(Titanic)
#-----------------------------------------------------------------
#rpart
library(rpart)
help("rpart")

fit = rpart(Survived~., data=Titanic, method="class")
plot(fit)
text(fit)
#Does not seem to load the graphics properly  but should have worked
#Source: https://trevorstephens.com/kaggle-titanic-tutorial/r-part-3-decision-trees/
#--------------------------------------------------------------------
#ctree
install.packages("partykit")
library(partykit)
trainset.ctree=ctree(Survived ~ ., Titanic)
trainset.ctree
plot(trainset.ctree)
#Source: https://rstudio-pubs-static.s3.amazonaws.com/241333_e3e176ecc7a44094a6bba75899a88f72.html
#----------------------------------------------------------------------------
#hclust
library(stats)
clusters=hclust(dist(Titanic))
clusters
plot(clusters)
#---------------------------------------------------------------------------------
#Random Forest
install.packages("randomForest")
library(randomForest)
fit = randomForest(Survived ~ .,
                      data=Titanic, 
                      importance=TRUE, 
                      ntree=2000)
#Random forest does not work with my version of R but the code should work 