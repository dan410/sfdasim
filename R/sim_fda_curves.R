#' simulates a collection of independent curves.
#'
#' For the specified basis functions, spatial dependence is incorporated through the covariance structure of the spatial fields connected to the coefficients. The coefficients of the first basis funciton is a simulated Gaussian random field with covariance model equal to the first specified covariance model and parameters equal to the first specified covariance parameters. The same is true for the remaining basis functions, which means that the number of basis functions should be equal to the number of covariance models specified, and the order in which they are specified is matters. The Gaussian random fields are simulated with the \code{grf} function in the \code{geoR} package. See \code{geoR} documentation for specification of covariance models.
#'
#' @param nbasis number of basis functions
#' @param nfuns number of curves to generate
#' @param type character specifying the type of basis functions to use. Default is \code{type="Cos"}.
#' @param basis.pars extra parameters used in the construction of basis functions
#' @return coef matrix containing coefficients of the basis functions. The number of columns is equal to the number of basis functions
#' @return basis.fns list of basis functions created by \code{create_basis}
#' @export


sim_fda_curves <- function(nbasis, nfuns, type="Cos", basis.pars=NULL){

  basis.fns <- create_basis(nBasis=nbasis, type=type, basis.pars=basis.pars)
  COEF <- sim_coef_unif(nbasis=nbasis, nfuns = nfuns)
  
  res <- list(coef = COEF, basis.fns = basis.fns)
  return(res)
}

##########################################################
