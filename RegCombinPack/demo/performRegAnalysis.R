set.seed(1801)
myX <- matrix(rnorm(n=60,mean=15),ncol=4) 
myY <- sample(1:100,15,replace=TRUE) 
myRegCombin <- fitRegCombin(myX, myY)
demoRegAnalysis <- performRegAnalysis(myRegCombin)
demoRegAnalysis
print(demoRegAnalysis)
