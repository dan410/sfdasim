
#' simulates the coefficients of the basis functions as independend uniform[0,1] variates#' 
#' 
#' @param nbasis number of basis functions representing each curve
#' @param nfuns number of curves
#' @return coef matrix with dimension \code{nfuns x nbasis} containing the coefficients 
sim_coef_unif <- function(nbasis, nfuns){
  coefs <- matrix(0, nrow = nfuns, ncol = nbasis)
  for( i in 1:nbasis){
    coefs[,i] <- runif(n = nfuns, min = -sqrt(3), max = sqrt(3)) 
  }
  return(coefs)
}