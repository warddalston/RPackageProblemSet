#' Plotting RegCombin objects
#' 
#' Plots some of the output of a RegCombin object
#' 
#' @usage plot(x,plot.int=TRUE,...)
#'
#' @param x an object of class `RegCombin'
#' @param plot.int a logical which determines whether or not to plot the intercept coefficient. Defaults to TRUE. 
#' @param ... arguements passed on to other functions.  
#' 
#' @details The plot method for objects of class `RegCombin' plots on the the average coefficient values for each of the covariates included in the fitting of regressions including all possible combinations of regression covariates.  Covariates are plotted along the x-axis.  Coefficient estimates are plotted along the y-axis.  The average coefficient estimate is plotted, along with lines indicating the coefficient estimates covered within 2 standard deviations of the mean estimate.  The second argument, plot.int, allows the user to opt not to plot the intercept's coefficient.  This is useful when the user is not interested in the intercept or when the intercept has a very different value then other covariates.    
#'
#' @examples
#' 
#' set.seed(1801)
#' myX <- matrix(rpois(n=60,lambda=15),ncol=4)
#' myY <- sample(1:100,15,replace=TRUE) 
#' myRegCombin <- fitRegCombin(myX, myY)
#' plot(myRegCombin,plot.int=FALSE)
#' @author Dalston G. Ward 
#' @seealso \code{\link{RegCombin-class}}
#' @rdname plotRegCombin
#' @export
setMethod(f="plot",
signature="RegCombin",
definition=function(x,plot.int=TRUE,...){
  if(plot.int==TRUE){
  vertic <- sapply(1:nrow(x@coefficients),function(y){mean(x@coefficients[y,],na.rm=T)})
  verticLOW <- vertic-2*sapply(1:nrow(x@coefficients),function(y){sd(x@coefficients[y,],na.rm=T)})
  verticHIGH <- vertic+2*sapply(1:nrow(x@coefficients),function(y){sd(x@coefficients[y,],na.rm=T)})
  horiz <-  1:nrow(x@coefficients)
  plot(x=horiz,y=vertic,
       pch=20,
       ylim=c(min(c(verticLOW,verticHIGH)),max(c(verticLOW,verticHIGH))),
       xaxt="n",
       ylab="Coefficiecnt Estimate",
       xlab="Covariate",
       main="Mean Coefficient Estimates Within 2 Standard Deviation Envolopes",...)
  arrows(x0=horiz,y0=verticLOW,x1=horiz,y1=verticHIGH,code=3,angle=90,length=.1)
  axis(side=1,at=horiz,labels=rownames(x@coefficients))
  } else {
    vertic <- sapply(2:nrow(x@coefficients),function(y){mean(x@coefficients[y,],na.rm=T)})
          verticLOW <- vertic-2*sapply(2:nrow(x@coefficients),function(y){sd(x@coefficients[y,],na.rm=T)})
          verticHIGH <- vertic+2*sapply(2:nrow(x@coefficients),function(y){sd(x@coefficients[y,],na.rm=T)})
          horiz <-  1:(nrow(x@coefficients)-1)
          plot(x=horiz,y=vertic,
               pch=20,
               ylim=c(min(c(verticLOW,verticHIGH)),max(c(verticLOW,verticHIGH))),
               xaxt="n",
               ylab="Coefficiecnt Estimate",
               xlab="Covariate",
               main="Mean Coefficient Estimates Within 2 Standard Deviation Envolopes",...)
          arrows(x0=horiz,y0=verticLOW,x1=horiz,y1=verticHIGH,code=3,angle=90,length=.1)
          axis(side=1,at=horiz,labels=rownames(x@coefficients)[-1])}
}
)
