#' evaluates the specified curves
#'
#' @param argvals vector of argument values
#' @param coef vector of matrix of coefficients of the basis functions
#' @param basis.fns list of basis functions
 eval_curves <- function(argvals, coef, basis.fns){
  xs <- argvals
  nBasis <- NCOL(coef)
  if(!is.matrix(coef)){
    X <- NULL
    for(i in 1:nBasis){
      X <- cbind(X,basis.fns[[i]](xs))
    }
    ys <- X%*%as.vector(coef)
    return(ys)
  }
  if(is.matrix(coef)){
    ncurve <- dim(coef)[1]
    ys <- matrix(0, nrow=length(argvals), ncol=ncurve)
    for(j in 1:ncurve){
      X <- NULL
      for(i in 1:nBasis){
        X <- cbind(X,basis.fns[[i]](xs))
      }
      ys[,j] <- X%*%as.vector(coef[j,])
      
    }
    return(ys)
  }
  
}