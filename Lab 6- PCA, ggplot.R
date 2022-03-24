#Lab 6

#PCA Using the Wine Dataset

#Load the Wine data
wine_data=read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",sep=',')
head(wine_data)
#Column names are not present
#Read the data set information online by googling

View(wine_data)

#First column is cultivar 
#Add the column names
colnames(wine_data)=c("CVS","Alcohol","Malic_Acid","Ash","Alkalinity_of_Ash","Magnesium","Total_Phenols","Flavonoids","NonFlavnoid_Phenols","Proanthocyanins","Color_Intenisty","Hue","0D280/0D315_of_Diluted_Wine","Proline")

nrow(wine_data)

summary(wine_data)

#Use heatmap to check correlations-dark=highly correlated
help(heatmap)
help(cor)

heatmap(cor(wine_data),Rowv=NA,Colv=NA)

#Want to identify the three variables based on the chemical data-declare 3 classes (Cv1,cv2,cv3) using factor function
help(factor)

#Make the classes
cultivarclasses=factor(wine_data$Cvs)
cultivarclasses

#Time for PCA
help(prcomp)
#Normalize using the scale function
help(scale)
#Normalize everything but the Cvs column
wine_data_PCA=prcomp(scale(wine_data[,-1]))

summary(wine_data_PCA)
#8 principal components explain 92% of the variance
#---------------------------------------------------------------------------------------------
#ggplot example

library(gcookbook)
ggplot(BOD, aes(x=Time, y=demand)) + geom_line()
BOD
BOD1 <- BOD # make a copy of the dataset
BOD1$Time <- factor(BOD1$Time)
ggplot(BOD1, aes(x=Time, y=demand, group=1)) + geom_line()
ggplot(BOD, aes(x=Time, y= demand)) +geom_line() + ylim(0, max(BOD$demand))
ggplot(BOD, aes(x=Time, y=demand)) +geom_line() + expand_limits(y=0)
# Adding points to a line graph
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + geom_point()
library(gcookbook) # For the data set
ggplot(worldpop, aes(x=Year, y=Population)) + geom_line() + geom_point()
# same with log-y axis
ggplot(worldpop, aes(x=Year, y=Population)) + geom_line() + geom_point() + scale_y_log10() 
#------------------------------------------------------------------------------------------------
