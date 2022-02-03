##When you filter out NAs, percentage filtered out tells quality of data, can you work around NA values (average, etc)

##In class Work 1/31/22

install.packages('ggplot2')
library(ggplot2)

data(diamonds)

View(diamonds)

help('ggplot')

install.packages('dplyr')
library(dplyr)

smaller = diamonds %>% filter(x<10)

#What is the variability of the three variables x,y,z?

ggplot(data=diamonds)+geom_histogram(mapping=aes(x=x),binwidth=.1)+xlim(0,10)
ggplot(data=diamonds)+geom_histogram(mapping=aes(x=y),binwidth=0.1) +xlim(0,10)
ggplot(data=diamonds)+geom_histogram(mapping=aes(x=z),binwidth=.1)+xlim(0,10)

##Length, width, and depth are arbitrary descriptors without a picture reference but x and y are very similar in their distribuiton and thuse would represent the circular portion of the diamond that should have two similar measurements which i will call width and depth whilst the z variable is smaller than the other which I will call the length

#What is the variability of price?

ggplot(data=diamonds)+geom_histogram(mapping=aes(x=price))
#the default bin width shows a right skewed distribution

attach(diamonds)

boxplot(price)
#There is a significant number of outliers

ggplot(data=diamonds)+geom_histogram(mapping=aes(x=price),binwidth=50)
#There is a strange missing gap 
ggplot(data=diamonds)+geom_histogram(mapping=aes(x=price),binwidth=100)
#The strange gap is still there, it appears to be between around 1100 to 1200

#Question, what is causing this gap?

#How many diamonds are 0.99 carat and how many are 1.0 carat

carat99 = diamonds %>% filter(carat==0.99)
dim(carat99)

carat1=diamonds%>% filter(carat==1)
dim(carat1)

#There are 23 0.99 carat diamonds and 1558 1 carat diamonds. This is likely because people are not going to want to buy a 0.99 carat and will want a 1 carat diamond. 

#What is the difference between coord_cartesian and xlim and ylim?
ggplot(data=diamonds)+geom_histogram(mapping=aes(x=price))
ggplot(data=diamonds)+geom_histogram(mapping=aes(x=price)) +coord_cartesian(xlim=c(0,15000))
ggplot(data=diamonds)+geom_histogram(mapping=aes(x=price)) +coord_cartesian(ylim=c(0,15000))
ggplot(data=diamonds)+geom_histogram(mapping=aes(x=price)) +xlim(0,15000)
ggplot(data=diamonds)+geom_histogram(mapping=aes(x=price)) +ylim(0,15000)
ggplot(data=diamonds)+geom_histogram(mapping=aes(x=price),binwidth=100) +coord_cartesian(xlim=c(0,1050))

#Have strange values? Drop the entire row
diamonds2= diamonds %>% filter(between(y,3,20))

#Or replace with missing values
diamonds2=diamonds%>% mutate(y=ifelse(y<3|y>20,NA,y))

ggplot(data=diamonds2,mapping=aes(x=x,y=y))+geom_point()                               
#To get rid of missing value warning
ggplot(data=diamonds2,mapping=aes(x=x,y=y))+geom_point(na.rm=TRUE)                               

#What happens to missing values in histograms and sum and mean?
ggplot(data=diamonds2)+geom_histogram(mapping=aes(x=y))
sum(diamonds2$y)
mean(diamonds2$y)

