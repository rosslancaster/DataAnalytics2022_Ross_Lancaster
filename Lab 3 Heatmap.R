#Lab 3

#creating a heatmap
set.seed(12345)
help(par)

#par can be used to set or query graphical parametrs
#Parameters can be set by specifying them as arguments
#to par in tag=value form, or by passing them as a list of tagged values

par(mar=rep(0.2,4))
data_Matrix=matrix(rnorm(400),nrow=40)
image(1:10,1:40,t(data_Matrix)[,nrow(data_Matrix):1])

help(heatmap)
help(rep)

par(mar=rep(0.2,4))
heatmap(data_Matrix)
#run heatmap, get dendrograms printed on both columns
#and the rows and still there is not real  immerging pattaernthat is interesting to us,
#is because there is no real interesting pattern undrelying in the data we generated

help(rbinom)

set.seed(678910)
for (i in 1:40){
  coin_Flip=rbinom(1,size=1,prob=0.5)
  if(coin_Flip){
    data_Matrix[i,]=data_Matrix[i,]+rep(c(0,3),each=5)
  }
}

par(mar=rep(0.2,4))
image(1:10,1:40,t(data_Matrix)[,nrow(data_Matrix):1])

par(mar=rep(0.2,4))
heatmap(data_Matrix)

##take a closer look at the patterns in rows and columns by looking at the marginal 
#means of the rows and columns
#Ten different column menas and forty different row means
hh=hclust(dist(data_Matrix))
data_Matrix_Ordered=data_Matrix[hh$order,]
par(mfrow=c(1,3))
image(t(data_Matrix_Ordered)[,nrow(data_Matrix_Ordered):1])
plot(rowMeans(data_Matrix_Ordered),40:1, ,xlab='The Row Mean',ylab='Row',pch=19)
plot(colMeans(data_Matrix_Ordered),xlab='Column',ylab='Column Mean',pch=19)
