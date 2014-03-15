#############################################################
## R Package Problem Set - Development File - Dalston Ward ##
#############################################################

## Load libraries and set working directory
library(devtools)
library(roxygen2)
setwd("/Users/clockbob1/Documents/WashU 2nd Year/Applied Stats Programming/March 6/RPackageProblemSet") #This will need to be changed to match your directory

## This is run once when the package strcuture is first created
create(path="./RegCombinPack", check=FALSE)

## This can be run many times as the code is updates
current.code <- as.package("RegCombinPack")
load_all(current.code)
document(current.code)

## Install the package
install(pkg=current.code, local=TRUE)

?plot
set.seed(1801)
myX <- matrix(rpois(n=60,lambda=15),ncol=4)
myY <- sample(1:100,15,replace=TRUE)
plot(fitRegCombin(myX, myY))

## Build a version of the package to share manually
build(current.code, path=getwd())
