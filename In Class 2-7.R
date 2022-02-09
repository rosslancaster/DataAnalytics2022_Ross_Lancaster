install.packages('gcookbook')
library(gcookbook)
library(ggplot2)

pg_mean

ggplot(pg_mean,aes(x=group,y=weight))+geom_bar(stat='identity')

BOD
#There is no time for entry for t=6

str(BOD)

ggplot(BOD,aes(x=Time,y=demand))+geom_bar(stat='identity')

#Convert time to a discrete variable with factor() function

ggplot(BOD,aes(x=factor(Time),y=demand))+geom_bar(stat='identity')

#R is weird and British and uses colour instead of color
help(geom_bar)
ggplot(BOD,aes(x=factor(Time),y=demand))+geom_bar(stat='identity',fill='darkred')

#View cabbage dataset
cabbage_exp

#Map date to x and fill with cultivar
ggplot(cabbage_exp, aes(x=Date,fill=Cultivar))+geom_bar(position='dodge')

ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+geom_bar(stat='identity')

#Making Bar Graph of Counts
ggplot(diamonds,aes(x=cut))+geom_bar()
#This is the same as using geom_bar(stat='bin)
data('diamonds')

ggplot(diamonds,aes(x=carat))+geom_histogram()

ggplot(diamonds,aes(x=carat))+geom_bar()

#Want to use different colors on the bars
View(uspopchange)

ups=subset(uspopchange,rank(Change)>40)
ups

ggplot(ups,aes(x=Abb,y=Change,fill=Region))+geom_bar(stat='identity')

#Expeirment with other charts

ggplot(ups,aes(x=Abb,y=Change,fill=Region))+geom_bin2d()
#This one is strange, small bars intead of full bars

ggplot(ups,aes(x=Abb,y=Change,fill=Region))+geom_col()

#Use scale filee brewer or scale fill manual to help change colors
ggplot(ups, aes(x=reorder(Abb,Change),y=Change,fill=Region))+geom_bar(stat='identity',colour='red')+scale_fill_manual(values=c("#669933","#FFCC66"))+xlab("US-States")
ggplot(ups, aes(x=reorder(Abb,Change),y=Change,fill=Region))+geom_bar(stat='identity',colour='purple')+scale_fill_manual(values=c("224455","#DDCC33"))

#try to make negative and positive values a different color

#make a new dataset
csub=subset(climate,source='Berkeley' & Year>= 1900)
csub

csub$pos=csub$Anomaly10y>=0
csub

ggplot(csub,aes(x=Year,y=Anomaly10y,fill=pos))+geom_bar(stat='identity',position='identity')
#Change color

ggplot(csub,aes(x=Year,y=Anomaly10y,fill=pos))+geom_bar(stat='identity',colour='black',size=0.25)+scale_fill_manual(values=c("#CCEEFF","#FFDDDD"),guide=FALSE)

#Adjust bar width and spacing, default bin width is 0.9
ggplot(pg_mean,aes(x=group,y=weight))+geom_bar(stat='identity')

#Narrow bars
ggplot(pg_mean,aes(x=group,y=weight))+geom_bar(stat='identity',width=0.5)

#make bars wider, maximum is 1
ggplot(pg_mean,aes(x=group,y=weight))+geom_bar(stat='identity',width=0.95)

#make bars of different width
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+geom_bar(stat='identity',width=0.5,position='dodge')

ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+geom_bar(stat='identity',width=0.5,position=position_dodge(0.7))

#Make a sketched bar graph
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+geom_bar(stat='identity')
cabbage_exp

ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+geom_bar(stat='identity')+guides(fill=guide_legend(reverse=TRUE))

#Add labels to graphs
ggplot(cabbage_exp,aes(x=interaction(Date,Cultivar),y=Weight))+geom_bar(stat='identity')+geom_text(aes(label=Weight),vjust=1.5,colour='white')ggp

#add y limits to be a little higher
ggplot(cabbage_exp,aes(x=interaction(Date,Cultivar),y=Weight))+geom_bar(stat='identity')+geom_text(aes(label=Weight),vjust=-0.2)+ylim(0,max(cabbage_exp$Weight)*1.05)

#map y positions slighly above the bar top
ggplot(cabbage_exp,aes(x=interaction(Date,Cultivar),y=Weight))+geom_bar(stat='identity')+geom_text(aes(y=Weight+0.1,label=Weight))

ggplot(cabbage_exp,aes(x=interaction(Date,Cultivar),y=Weight))+geom_bar(stat='identity',position='dodge')+geom_text(aes(label=Weight),vjust=1.5,colour='white',position=position_dodge(.9),size=3)


#Make a Cleveland dot plot
#Take top 25 hitters
tophit=tophitters2001[1:25,]
tophit

ggplot(tophit,aes(x=avg,y=name))+geom_point()
tophit[,c('name','lg','avg')]
ggplot(tophit,aes(x=avg,y=reorder(name,avg)))+geom_point(size=3,colour='red')+theme_bw()+theme(panel.grid.major.x=element_blank(),panel.grid.minor.x=element_blank(),panel.grid.major.y=element_line(colour='grey60',linetype='dashed'))

ggplot(tophit,aes(x=avg,y=reorder(name,avg)))+geom_point(size=2.5,colour='blue')+theme_classic()+theme(panel.grid.major.x=element_blank(),panel.grid.minor.x=element_blank(),panel.grid.major.y=element_line(colour='grey60',linetype='twodash'))

#get the names sorted by lg then by avg
nameorder=tophit$name[order(tophit$lg,tophit$avg)]

#Turn name into factor with levels in the order of nameorder
tophit$name=factor(tophit$name,levels=nameorder)

ggplot(tophit,aes(x=avg,y=name))+geom_segment(aes(yend=name),xend=0,colour='grey70')+geom_point(size=3,aes(colour=lg))+scale_color_brewer(palette='Set1',limits=c('NL','AL'))+theme_bw()+theme(panel.grid.major.y=element_blank(),legend.position=c(1,0.55),legend.justification=c(1,0.5))
