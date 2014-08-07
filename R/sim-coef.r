
#' simulates the coefficients of the basis functions as independend Gaussian random fields
#' 
#' simulates a Gaussian random field for each basis function given user specified covariance function
#' and covariance parameters. The model specification corresponds to GeoR package.
#' 
#' @param nfields number of fields to simulate (one for each basis function)
#' @param grid.xlim vector of length 2 specifying the xlim for the grid. Only used if locs is not specified.
#' @param grid.ylim vector of length 2 specifying the ylim for the grid. Only used if locs is not specified.
#' @param grid.dim vector with 2 numbers specifying the dimension of the rectangular grid. Only used if locs is not specified.
#' @param cov.model name of covariance model(s) connected to each basis function.
#' @param cov.pars a vector with 2 elements or an n x 2 matrix with values of the 
#' covariance parameters sigma^2 (partial sill) and phi (range parameter). If a vector, 
#' the elements are the values of sigma^2 and phi, respectively. If a matrix, 
#' corresponding to a model with several structures, the values of sigma^2 are in 
#' the first column and the values of phi are in the second.
#' @param locs nx2 matrix of locations
#' @param ... other parameters sent to grf()
#' @return coef list containing the coefficients ...
#' @return locs nx2 matrix of locations
#' @return cov.model name of covariance model(s) connected to each basis function.
#' @return cov.pars covariance parameters corresponding to the covariance models.
sim_coef <- function(nfields, grid.dim, grid.xlim, grid.ylim, cov.model, cov.pars, locs = NULL, ...){
  
  if(is.null(locs)){# create grid of locations
  	nx <- grid.dim[1]
  	ny <- grid.dim[2]
  	locs <- expand.grid(seq(from=grid.xlim[1], grid.xlim[2], length.out = nx), seq(from = grid.ylim[1], to=grid.ylim[2],length.out=ny))
  }
  coef.fields <- NULL
  for( i in 1:nfields){
    sim <-  grf(n=NROW(locs), grid=locs, cov.model=cov.model[i], cov.pars=cov.pars[i,],...)  
    coef.fields <- cbind(coef.fields, sim$data)
    #image(sim); Sys.sleep(2)
  }
  res <- list(coef=coef.fields, locs=locs, cov.model=cov.model, cov.pars=cov.pars)
  return(res)
}