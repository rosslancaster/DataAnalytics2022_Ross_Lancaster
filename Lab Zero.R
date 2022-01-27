data=read.csv(file.choose(),header=TRUE)
data

names(data) <- as.matrix(data[1, ])
data<- data[-1,]

data[]=lapply(data, function(x) 
type.convert(as.character(x)))
data
View(data)

dim(data)
is.na(data)
summary(data)
names(data)
summary(data$Landarea)
fivenum(data$Landarea)

shapiro.test(data$Landarea)

boxplot(data$Landarea)
stem(data$Landarea)
hist(data$Landarea)

hist(data$EPI, seq(30,95,1), prob=TRUE)
lines(density(data$EPI,na.rm=TRUE,bw='SJ'))
rug(data$EPI)

xn=seq(30,95,1)
qn=dnorm(xn,mean=63,sd=5,log=FALSE)
lines(xn,qn)
lines(xn,.4*qn)
ln=dnorm(xn,mean=44,sd=5,log=FALSE)
lines(xn,.26*ln)
