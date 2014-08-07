#' generates data from a family of curves
#' 
#' generates a functional data set by evaluating curves and adding Gaussian noise.
#' 
#' @param coef matrix or data frame containing the coefficients of the basis functions. The number of columns is equal to the number of basis functions
#' @param basis.fns basis functions given by the output of create_basis
#' @param sigma0 standard deviation of the noise term.
#' @param m number of observations per curve
#' @param pts vector of values where the curves will be evaluated
#' @param DIST distribution function used to generate observatin locations for each curve. Default is runif().
#' @param ... additional parameter passed to dist function
#' @return ID integer identifying a single curve
#' @return Time argument values where curves are evaluated
#' @return X response variable
#' @export
sim_fda_data <- function(coef, basis.fns, sigma0, m=NULL, pts = NULL, DIST = runif, ...){
  if ( !is.null(pts) && (!is.vector(pts)||!is.numeric(pts))){
    stop(paste("pts must be either NULL or numeric vector, pts is ", pts))
  }
  if (is.null(m) & is.null(pts)){
    stop(paste("must specify number of observations per curve by specifying m or pts"))
  }
  
  ncurve <- NROW(coef)
  nBasis <- NCOL(coef)
  if (!is.null(pts)){
    ID <- rep(1:ncurve, each=length(pts))
  }
  else{
    ID <- rep(1:ncurve, each=m)
  }
  Time <- numeric(length(ID))
  X <- numeric(length(ID))
  
  
  for(i in 1:ncurve){
    if( is.null(pts) ){Time[ID==i] <- DIST(m, ...)}
    if(is.vector(pts) & is.numeric(pts)){Time[ID==i] <- pts}
    
    ys <- NULL
    for( j in 1:nBasis){
      ys <- cbind(ys, basis.fns[[j]](Time[ID==i]))
    }
    ys <- ys%*%as.vector(coef[i,])
    X[ID==i] <- ys + rnorm(n=length(ys), sd=sigma0)
  }
  dat <- data.frame( ID=ID, Time=Time, X=X)
  return(dat)
}
