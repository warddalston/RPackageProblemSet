#############################################################
## R Package Problem Set - Development File - Dalston Ward ##
#############################################################

## Load libraries and set working directory
library(devtools)
library(roxygen2)
setwd("/Users/clockbob1/Documents/WashU 2nd Year/Applied Stats Programming/March 6/RPackageProblemSet") #This will need to be changed to match your directory

## This is run once when the package strcuture is first created
create(path="./RegCombinPack", check=FALSE)

## write the example data set
set.seed(1801)
dataX <- matrix(rpois(n=60,lambda=26),ncol=4)
dataY <- sample(1:100,15,replace=TRUE)
RegCombinPackData <- cbind(dataY,dataX)
colnames(RegCombinPackData) <- c("Y","Variable 1","Variable 2","Variable 3","Variable 4")
save(RegCombinPackData,file="./RegCombinPack/data/RegCombinPackData.rda")

## This can be run many times as the code is updates
current.code <- as.package("RegCombinPack")
load_all(current.code)
document(current.code)

## Install the package
install(pkg=current.code, local=TRUE)

#code to test! 
?RegCombinPack
set.seed(1801)
myX <- matrix(rpois(n=60,lambda=15),ncol=4)
myY <- sample(1:100,15,replace=TRUE)
toy <- fitRegCombin(myX, myY)
emptytoy <- new("RegCombin")
emptytoy
print(toy)
show(toy) 
getRegCombin(toy)
getRegCombinInput(toy)
shinytoy <- performRegAnalysis(toy)
print(shinytoy)
show(shinytoy)
plot(toy,plot.int=FALSE)
plot(shinytoy)
data(RegCombinPackData)
fitRegCombin(RegCombinPackData[,2:5],RegCombinPackData[,1])
demo(fitRegCombin)
demo(performRegAnalysis)

#see what things may be wrong...
check(current.code)

## Build a version of the package to share manually
build(current.code, path=getwd())
