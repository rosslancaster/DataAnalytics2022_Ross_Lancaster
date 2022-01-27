#Creating a dataframe

days=c('Mon','Tue','Wed','Thur','Fri','Sat','Sun') #days
temp=c(28,30.5,32,31.2,29.3,27.9,26.4) # Temperature in F during the winter
snowed=c('T','T','F','F','T','T','F') #Snowed on that T=True, F=False
snowed
help('data.frame')
RPI_Weather_Week=data.frame(days,temp,snowed) #creating the data frame using data.frame() function

RPI_Weather_Week
head(RPI_Weather_Week) #head of the data frame

str(RPI_Weather_Week)

summary(RPI_Weather_Week)

RPI_Weather_Week[1,] #showing the 1st row and all the columns
RPI_Weather_Week[,1] #showing the 1st column and all the rows

RPI_Weather_Week[,'snowed']
RPI_Weather_Week[,'days']
RPI_Weather_Week[,'temp']
RPI_Weather_Week[1:5,c('days','temp')]
RPI_Weather_Week$temp
subset(RPI_Weather_Week, subset=snowed==TRUE)

sorted.snowed=order(RPI_Weather_Week['snowed'])
sorted.snowed
RPI_Weather_Week[sorted.snowed,]

#Sort temperatures descending
dec.snow=order(-RPI_Weather_Week$temp)
dec.snow

empty.DataFrame=data.frame()
v1=1:10
v1
letters
v2=letters[1:10]
df=data.frame(col.name1=v1,col.name2=v2)
df

#importing data and exporting data
#writing to a csv file
write.csv(df,file='saved_df1.csv')
df2=read.csv('saved_df1.csv')
df2

gpw3=read.csv(file.choose(),header=TRUE)
gpw3

epi=read.csv(file.choose(), header=TRUE)
epi
E
View(epi)

names(epi) <- as.matrix(epi[1, ])
epi<- epi[-1,]

epi[]=lapply(epi, function(x) E
  type.convert(as.character(x)))
epi
View(epi)

attach(epi)
fix(EPI)
epi

tf=is.na(EPI)
E=EPI[!tf]

summary(E)

summary(epi$EPI)
fivenum(epi$EPI,na.rm=TRUE)
stem(epi$EPI)
hist(epi$EPI)
hist(epi$EPI,seq(30,95,1),main='Histogram of EPI', prob=TRUE)
lines(density(epi$EPI,na.rm=TRUE,bw='SJ'))
rug(epi$EPI)

help(hist)

plot(ecdf(epi$EPI),do.points=FALSE,verticals=TRUE)

par(pty='s')
qqnorm(epi$EPI);qqline(epi$EPI)

qqplot(qt(ppoints(250),df=5),x,xlab='Q-Q Plot for t dsn')
qqline(x)

attach(epi)
fix(DALY)

tf=is.na(DALY)
E2=EPI[!tf]
summary(E2)

summary(epi$DALY)
fivenum(epi$DALY,na.rm=TRUE)
stem(epi$DALY)
hist(epi$DALY)
hist(epi$DALY,seq(0,95,1),main='Histogram of DALY', prob=TRUE)
lines(density(epi$DALY,na.rm=TRUE,bw='SJ'))
rug(epi$DALY)



