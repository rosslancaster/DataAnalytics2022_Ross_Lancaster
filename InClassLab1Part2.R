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

