#Lab 2

#Load EPI Dataset a
epi=read.csv(file.choose(), header=TRUE)
epi
E
View(epi)

#Get column names out of the first row
names(epi) <- as.matrix(epi[1, ])
epi<- epi[-1,]

epi[]=lapply(epi, function(x) E
             type.convert(as.character(x)))
epi
View(epi)

attach(epi)

tf=is.na(EPI)
E=EPI[!tf]


plot(ecdf(EPI),do.points=FALSE,verticals=TRUE)

help('qqnorm')

par(pty='s')
qqnorm(EPI)
qqline(EPI)
qqplot(qt(ppoints(250),df=5),x,xlab='Q-Q plot for t dsn')
qqline(x)

plot(ecdf(epi$EPI),do.points=FALSE.verticals=TRUE)
plot(ecdf(epi$EPI),do.points=TRUE,verticals=TRUE)

par(pty='s')

x=seq(30,95,1)
qqplot(qt(ppoints(250),df=5),x,xlab='Q-Q plot for t dsn')

x2=seq(30,95,2)
qqplot(qt(ppoints(250),df=5),x2,xlab='Q-Q plot for t dsn')

#Look at other variables, DALY, etc.
plot(ecdf(DALY),do.points=FALSE,verticals=TRUE)

par(pty='s')
qqnorm(DALY)
qqline(DALY)
qqplot(qt(ppoints(250),df=5),x,xlab='Q-Q plot for t dsn')
qqline(x)

#AIR_H
par(pty='s')
qqnorm(AIR_H)
qqline(AIR_H)
qqplot(qt(ppoints(250),df=5),x,xlab='Q-Q plot for t dsn')
qqline(x)

#Compare variables
boxplot(EPI,DALY)

#Intercompare
boxplot(EPI,ENVHEALTH)
boxplot(EPI,AIR_H)
boxplot(EPI,ECOSYSTEM)
boxplot(EPI,WATER_H)
boxplot(EPI,BIODIVERSITY)

View(BIODIVERSITY)

#Load multivariate
multi=read.csv(file.choose(), header=TRUE)
multi

attach(multi)

#Try linear regression between homeowners and immigrants
help(lm)
mm=lm(Homeowners~Immigrant)
mm

#Explore multivariate dataset
head(multi)

summary(mm)$coef

plot(Homeowners~Immigrant)

help(abline)

abline(mm)
abline(mm,col=2,lwd=3)

newimmigrantdata=data.frame(Immigrant=c(0,20))
mm %>% predict(newimmigrantdata)
#I cannot figure out why R will not recognize %>% as a function

abline(mm)
abline(mm,col=3,lwd=3)

attributes(mm)
mm$coefficients

#Creating Plots
plot(mtcars$wt,mtcars$mpg)

library(ggplot2)
qplot(mtcars$wt,mtcars$mpg)
qplot(wt,mpg,data=mtcars)
#Both are the same

ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()

#Both of these look the same
plot(pressure$temperature,pressure$presssure,type='l')
points(pressure$temperature,pressure$pressure)

lines(pressure$temperature,pressure$pressure/2,col='red')
points(pressure$temperature,pressure$pressure/2,col='blue')

qplot(pressure$temperature,pressure$pressure,geom='line')
qplot(temperature,pressure,data=pressure,geom='line')

ggplot(pressure,aes(x=temperature,y=pressure))+geom_line()+geom_point()

#Creating bar graphs
barplot(BOD$demand,names.org=BOD$Time)
table(mtcars$cyl)

#Generate table of counts
barplot(table(mtcars$cyl))

#Cyl is continuous here
qplot(mtcars$cyl)

#Treat cyl as discrete
qplot(factor(mtcars$cyl))

#Bar graph of counts
qplot(factor(cyl),data=mtcars)
ggplot(mtcars,aes(x=factor(cyl)))+geom_bar()

#Create histograms
hist(mtcars$mpg)
#Change sizes of bins
hist(mtcars$mpg,breaks=10)
hist(mtcars$mpg,breaks=5)
hist(mtcars$mpg,breaks=12)

qplot(mpg,data=mtcars,binwidth=4)
ggplot(mtcars,aes(x=mpg))+geom_histogram(binwidth=4)
ggplot(mtcars,aes(x=mpg))+geom_histogram(binwidth=5)

#Create box plots
(plot(ToothGrowth$supp,ToothGrowth$len))

boxplot(len~supp,data=ToothGrowth)

ggplot(ToothGrowth,aes(x=supp,y=len))+geom_boxplot()
