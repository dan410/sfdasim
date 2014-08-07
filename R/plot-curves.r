#' Plots curves from a functional dataset with specified basis functions and coefficients
#' 
#' Given basis functions created by create_basis() and a matrix of coefficients
#' will plot the family of curves on a single plot
#' 
#' @param coef vector of matrix of coefficients. Rows correspond to the coefficients of a single functional observation
#' @param basis.fns list of basis functions created by \code{create_basis}
#' @param xlim vector of length 2
#' @param ylim vector of length 2
#' @param xlab x label
#' @param ylab y label 
#' @param ... arguments passed to plot
#' @export
plot_curves <- function(coef, basis.fns, xlim=c(0,1), ylim=c(-2,2), xlab="", ylab="", ...){
  xs <- seq(from=xlim[1], to=xlim[2], length=1000)
  nBasis <- NCOL(coef)
  if(!is.matrix(coef)){
    X <- NULL
    for(i in 1:nBasis){
      X <- cbind(X,basis.fns[[i]](xs))
    }
    ys <- X%*%as.vector(coef)
    plot(xs, ys , type="l", xlim=xlim, ylim = ylim, xlab = xlab, ylab = ylab, ...)
  }
  if(is.matrix(coef)){
    ncurve <- dim(coef)[1]
    plot(xlim, ylim, type="n", xlab=xlab, ylab=ylab)
    for(j in 1:ncurve){
      X <- NULL
      for(i in 1:nBasis){
        X <- cbind(X,basis.fns[[i]](xs))
      }
      ys <- X%*%as.vector(coef[j,])
      points(xs, ys, type="l", xlim=xlim, ylim=ylim,...)
      
    }
  }
}
