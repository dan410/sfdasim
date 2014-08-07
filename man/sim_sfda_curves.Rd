\name{sim_sfda_curves}
\alias{sim_sfda_curves}
\title{simulates a collection of curves with spatial dependendence.}
\usage{
sim_sfda_curves(nBasis, cov.model, cov.pars, ..., type = "Cos",
  basis.pars = NULL, grid.dim = c(10, 10), grid.xlim = c(0, 1),
  grid.ylim = c(0, 1), locs = NULL, write = FALSE, file = NULL)
}
\arguments{
  \item{nBasis}{number of basis functions}

  \item{cov.model}{character vector specifying covariance
  function(s). The length of this vector should equal the
  number of basis functions.}

  \item{cov.pars}{parameters for the covariance function(s)
  specified in cov.model}

  \item{...}{additional arguments sent to \code{grf}. See
  help for \code{grf} in \code{goeR} for details.}

  \item{type}{character specifying the type of basis
  functions to use. Default is \code{type="Cos"}.}

  \item{basis.pars}{extra parameters used in the
  construction of basis functions}

  \item{grid.dim}{vector with 2 numbers specifying the
  dimension of the rectangular grid. Only used if
  \code{locs} is not specified.}

  \item{grid.xlim}{vector of length 2 specifying the xlim
  for the grid. Only used if \code{locs} is not specified.}

  \item{grid.ylim}{vector of length 2 specifying the ylim
  for the grid. Only used if \code{locs} is not specified.}

  \item{locs}{nx2 matrix of locations. If NULL, then grid
  values are used to create locations}

  \item{write}{logical. If \code{write=TRUE} will write
  results to \code{file}}

  \item{file}{character specifying the file name. Only used
  if \code{write = TRUE}}
}
\value{
locs matrix containing spatial locations of the curves

coef matrix containing coefficients of the basis functions.
The number of columns is equal to the number of basis
functions

basis.fns list of basis functions created by
\code{create_basis}
}
\description{
For the specified basis functions, spatial dependence is
incorporated through the covariance structure of the
spatial fields connected to the coefficients. The
coefficients of the first basis funciton is a simulated
Gaussian random field with covariance model equal to the
first specified covariance model and parameters equal to
the first specified covariance parameters. The same is true
for the remaining basis functions, which means that the
number of basis functions should be equal to the number of
covariance models specified, and the order in which they
are specified is matters. The Gaussian random fields are
simulated with the \code{grf} function in the \code{geoR}
package. See \code{geoR} documentation for specification of
covariance models.
}
\examples{
curves <- sim_sfda_curves(nBasis = 2, cov.model = c("gaussian", "exponential"), cov.pars = rbind(c(1, 0.5), c(1, .3)), type="Cos", basis.pars = 2, locs = expand.grid(1:5/5, 1:10/10))
plot_curves(curves$coef, curves$basis.fns, ylim=c(-3,3))

sim.data <- sim_sfda_data(curves$locs, curves$coef, curves$basis.fns, sigma0=0.4, m = 10)
## plot_data(sim.data)
}

