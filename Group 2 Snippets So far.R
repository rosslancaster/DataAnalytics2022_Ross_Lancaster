#Group 2 Code Snippets

# Lab spm
install.packages("car")
require(car)
scatterplotMatrix(iris)
# and
scatterplotMatrix(swiss)
#-------------------------------------
#Lab 1 G pairs 1
install.packages("gpairs")
library(gpairs)
allexamples <- FALSE

y <- data.frame(A=c(rep("red", 100), rep("blue", 100)),
                B=c(rnorm(100),round(rnorm(100,5,1),1)), C=runif(200),
                D=c(rep("big", 150), rep("small", 50)),
                E=rnorm(200))
gpairs(y)

data(iris)
gpairs(iris)
if (allexamples) {
  gpairs(iris, upper.pars = list(scatter = 'stats'),
         scatter.pars = list(pch = substr(as.character(iris$Species), 1, 1),
                             col = as.numeric(iris$Species)),
         stat.pars = list(verbose = FALSE))
  gpairs(iris, lower.pars = list(scatter = 'corrgram'),
         upper.pars = list(conditional = 'boxplot', scatter = 'loess'),
         scatter.pars = list(pch = 20))
}

data(Leaves)
gpairs(Leaves[1:10], lower.pars = list(scatter = 'loess'))
if (allexamples) {
  gpairs(Leaves[1:10], upper.pars = list(scatter = 'stats'),
         lower.pars = list(scatter = 'corrgram'),
         stat.pars = list(verbose = FALSE), gap = 0)
  corrgram(Leaves[,-33])
}

runexample <- FALSE
if (runexample) {
  data(NewHavenResidential)
  gpairs(NewHavenResidential)
}
#--------------------------------------------------------------------
#Lab 1 Splom
require(lattice)
super.sym <- trellis.par.get("superpose.symbol")
splom(~iris[1:4], groups = Species, data = iris,
      panel = panel.superpose,
      key = list(title = "Three Varieties of Iris",
                 columns = 3, 
                 points = list(pch = super.sym$pch[1:3],
                               col = super.sym$col[1:3]),
                 text = list(c("Setosa", "Versicolor", "Virginica"))))
splom(~iris[1:3]|Species, data = iris, 
      layout=c(2,2), pscales = 0,
      varnames = c("Sepal\nLength", "Sepal\nWidth", "Petal\nLength"),
      page = function(...) {
        ltext(x = seq(.6, .8, length.out = 4), 
              y = seq(.9, .6, length.out = 4), 
              labels = c("Three", "Varieties", "of", "Iris"),
              cex = 2)
      })
parallelplot(~iris[1:4] | Species, iris) 
parallelplot(~iris[1:4], iris, groups = Species,
             horizontal.axis = FALSE, scales = list(x = list(rot = 90)))
#---------------------------------------------------------------------------
#Lab 3 Rpart1
require(rpart)
Swiss_rpart <- rpart(Fertility ~ Agriculture + Education + Catholic, data = swiss)
plot(Swiss_rpart) # try some different plot options
text(Swiss_rpart) # try some different text options
#-----------------------------------------------------------
#Lab 3 Rpart2---Not done
# Regression Tree Example
require(rpart)
# build the  tree
fitM <- rpart(Mileage~Price + Country + Reliability + Type, method="anova", data=cu.summary)
printcp(fitM) # display the results
plotcp(fitM)
summary(fitM)
par(mfrow=c(1,2)) 
rsq.rpart(fitM) # visualize cross-validation results
# plot tree
plot(fitM, uniform=TRUE, main="Regression Tree for Mileage ")
text(fitM, use.n=TRUE, all=TRUE, cex=.8)
# prune the tree
pfitM<- prune(fitM, cp=0.01160389) # from cptable??? adjust this to see the effect
# plot the pruned tree
plot(pfitM, uniform=TRUE, main="Pruned Regression Tree for Mileage")
text(pfitM, use.n=TRUE, all=TRUE, cex=.8)
post(pfitM, title = "Pruned Regression Tree for Mileage",file = ptree2,".ps")

help(post)
