#' simulates a collection of curves with spatial dependendence.
#'
#' For the specified basis functions, spatial dependence is incorporated through the covariance structure of the spatial fields connected to the coefficients. The coefficients of the first basis funciton is a simulated Gaussian random field with covariance model equal to the first specified covariance model and parameters equal to the first specified covariance parameters. The same is true for the remaining basis functions, which means that the number of basis functions should be equal to the number of covariance models specified, and the order in which they are specified is matters. The Gaussian random fields are simulated with the \code{grf} function in the \code{geoR} package. See \code{geoR} documentation for specification of covariance models.
#'
#' @param nBasis number of basis functions
#' @param cov.model character vector specifying covariance function(s). The length of this vector should equal the number of basis functions.
#' @param cov.pars parameters for the covariance function(s) specified in cov.model
#' @param ... additional arguments sent to \code{grf}. See help for \code{grf} in \code{goeR} for details.
#' @param type character specifying the type of basis functions to use. Default is \code{type="Cos"}.
#' @param basis.pars extra parameters used in the construction of basis functions
#' @param grid.dim vector with 2 numbers specifying the dimension of the rectangular grid. Only used if \code{locs} is not specified.
#' @param grid.xlim vector of length 2 specifying the xlim for the grid. Only used if \code{locs} is not specified.
#' @param grid.ylim vector of length 2 specifying the ylim for the grid. Only used if \code{locs} is not specified.
#' @param locs nx2 matrix of locations. If NULL, then grid values are used to create locations
#' @param write logical. If \code{write=TRUE} will write results to \code{file}
#' @param file character specifying the file name. Only used if \code{write = TRUE}
#' @return locs matrix containing spatial locations of the curves
#' @return coef matrix containing coefficients of the basis functions. The number of columns is equal to the number of basis functions
#' @return basis.fns list of basis functions created by \code{create_basis}
#' @export
#' @examples
#' curves <- sim_sfda_curves(nBasis = 2, cov.model = c("gaussian", "exponential"), cov.pars = rbind(c(1, 0.5), c(1, .3)), type="Cos", basis.pars = 2, locs = expand.grid(1:5/5, 1:10/10))					  
#' plot_curves(curves$coef, curves$basis.fns, ylim=c(-3,3))
#'
#' sim.data <- sim_sfda_data(curves$locs, curves$coef, curves$basis.fns, sigma0=0.4, m = 10)
#' ## plot_data(sim.data)

sim_sfda_curves <- function(nBasis, cov.model, cov.pars,..., type="Cos", basis.pars=NULL, grid.dim=c(10,10), grid.xlim=c(0,1), grid.ylim=c(0,1),locs=NULL, write=FALSE, file=NULL){

  basis.fns <- create_basis(nBasis=nBasis, type=type, basis.pars=basis.pars)
  COEF <- sim_coef(nfields=nBasis, grid.dim=grid.dim, grid.xlim=grid.xlim, grid.ylim=grid.ylim, cov.model=cov.model, cov.pars=cov.pars, locs = locs,...)
  
  res <- list(locs = COEF$locs, coef = COEF$coef, basis.fns = basis.fns)
  if(write){
    if(is.null(file)){file=paste("simulated spatial curves", date())}
    save(res, file=file)
  }
  return(res)
}

##########################################################
