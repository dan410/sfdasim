#' Create a list of basis functions
#' 
#' Create a list of basis functions...
#' 
#' @param nBasis number of functions to construct
#' @param type type of functions to construc e.g., "Cos", "Legendre" (type = "Legendre" has not been implemented yet)
#' @param basis.pars parameters used in construction of the basis functions
#' @return list of the basis functions
create_basis <- function(nBasis, type="Cos", basis.pars){
  
  if(type=="Cos"){ # must specify 'alpha' in the top-level call
  	if(is.null(basis.pars)){stop("For type: Cos, alpha must be specified in basis.pars argument")}
  	alpha <- basis.pars[1] # alpha controls the smoothness of the process
    fns <- lapply(1:nBasis, function(i){
      retval <- function(t,i){(-1)^(i+1)*i^(-alpha)*cos(i*pi*t)}
      formals(retval)$i <- i
      return(retval)   
    })
  }
  if(type=="Legendre"){}
  
  return(fns)
}
