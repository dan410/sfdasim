#' generates data from a family of curves
#' 
#' generates a functional data set by evaluating curves and adding Gaussian noise.
#' 
#' @param locs N by 2 matrix of spatial locations
#' @param coef matrix or data frame containing the coefficients of the basis functions. The number of columns is equal to the number of basis functions
#' @param basis.fns basis functions given by the output of create_basis
#' @param sigma0 standard deviation of the noise term.
#' @param m number of observations per curve
#' @param pts vector of values where the curves will be evaluated
#' @param DIST distribution function used to generate observatin locations for each curve. Default is runif().
#' @param ... additional parameter passed to dist function
#' @return ID integer identifying a single curve
#' @return locs spatial location of each curve
#' @return Time argument values where curves are evaluated
#' @return X response variable
#' @export
#' @examples
#' curves <- sim_sfda_curves(nBasis = 2, cov.model = c("gaussian", "exponential"), cov.pars = rbind(c(1, 0.5), c(1, .3)), type="Cos", basis.pars = 2, locs = expand.grid(1:5/5, 1:10/10))					  
#' plot_curves(curves$coef, curves$basis.fns, ylim=c(-3,3))
#'
#' sim.data <- sim_sfda_data(curves$locs, curves$coef, curves$basis.fns, sigma0=0.4, m = 10)
#' ## plot_data(sim.data)
sim_sfda_data <- function(locs, coef, basis.fns, sigma0, m=NULL, pts = NULL, DIST = runif, ...){
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
    Locs <- apply(locs, 2, rep, each=length(pts))
  }
  else{
    ID <- rep(1:ncurve, each=m)
    Locs <- apply(locs, 2, rep, each=m)
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
  dat <- data.frame( ID=ID,locs = Locs, Time=Time, X=X)
  return(dat)
}
