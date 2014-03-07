#' Analyzing Covariate Choice Consequences
#'
#' Regresses all possible combinations of a covariates on an outcome variable
#'
#' @param X A matrix or data frame object with no missing values
#' @param y A numeric object with the same number of elements as \code{X} has rows.
#'
#' @return An object of class RegAnalysis containing
#'  \item{coefficients}{A n by (2^n)-1 matrix of coefficient values, where n is the number of columns in \code{X}.  Each column in this object represeents the output of a single model, each row represents a given variable in \code{X}.}
#'  \item{R2}{A numeric vector of length (2^n)-1 of R^2 values, where n is the number of columns in \code{X}.}
#'  \item{X}{The first object input} 
#'  \item{y}{The second object input}
#' @author Dalston G. Ward
#' @note The reason that the number of combinations is (2^n)-1 instead of 2^n is that one cannot fit a regression with no covariates and no intercept (which is the combination of none of the columns of \code{X}.
#' @examples
#' 
#' set.seed(1801)
#' myX <- matrix(rpois(n=60,lambda=15),ncol=4)
#' myX <- cbind(1,myX)
#' myY <- sample(1:100,15,replace=TRUE) 
#' fitRegAnalysis(myX, myY)
#' @rdname fitRegAnalysis
#' @aliases fitRegAnalysis,ANY-method
#' @export
setGeneric(name="fitRegAnalysis",
           def=function(X, y, ...)
           {standardGeneric("fitRegAnalysis")}
)

#' @export
setMethod(f="fitRegAnalysis",
          definition=function(X, y, ...){
          #The method first creates a logical matrix specifying which variables are in each of the combinations.  There are (2^n)-1 possible combinations of covariates (including the intercept as a covariate).  The reason for the -1 is that it is not possible to fit a model with no intercepts and no covariates!  
          selectorList <- sapply(1:ncol(X),function(x){ 
              apply(combn(1:ncol(X),x), 2, function(z){
                CurrentCombn <- logical(length=5)
                CurrentCombn[z] <- TRUE
                return(CurrentCombn)
                }) #close the apply
              }) #close the sapply
          selectorMatrix <- matrix(unlist(selectorList),nrow=ncol(X)) #turn the list output in a matrix.  
            
          #The next section creates the objects that will be in the output.  They have informative names and are sized based on parameter inputs.   
          Coefficients <- matrix(nrow=ncol(X),ncol=2^ncol(X)-1)
          R.squareds <- vector(length=2^ncol(X)-1)
          colnames(Coefficients) <- names(R.squareds) <- apply(selectorMatrix,2,function(x){ paste("Vars:",paste(which(x==TRUE),collapse=","))})
          rownames(Coefficients) <- paste("Variable", 1:5, sep=" ")
          
          #The final section of this function does the actual regression fitting.  Using a_ply it fits a regression for each column of the selectorMatrix where the covariates are the based on the "TRUE" values for a given column.  Then, it writes the coefficent values and R^2 value to the appropriate element of the output objects using "<<-" to force writing these to the first variable matching the specified name in a parent environment.  This is basically R-cheating.   
          a_ply(.data=1:ncol(selectorMatrix),.margins=1,.fun=function(z){
            mod <- lm(y~X[,selectorMatrix[,z]]-1)
            Coefficients[selectorMatrix[,z],z] <<- mod[[1]] 
            R.squareds[z] <<- summary(mod)[["r.squared"]]
          })
          
          #it returns the output generated above as the input of the slots of an S4 object of "RegAnalysis" class
          return(new("RegAnalysis", coefficients=Coefficients,R2=R.squareds,X=X,y=y))
          }
)

set.seed(1801)
myX <- matrix(rpois(n=60,lambda=15),ncol=4)
myX <- cbind(1,myX)
myY <- sample(1:100,15,replace=TRUE) 
fitRegAnalysis(myX, myY)
