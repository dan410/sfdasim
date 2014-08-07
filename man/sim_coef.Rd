\name{sim_coef}
\alias{sim_coef}
\title{simulates the coefficients of the basis functions as independend Gaussian random fields}
\usage{
sim_coef(nfields, grid.dim, grid.xlim, grid.ylim, cov.model, cov.pars,
  locs = NULL, ...)
}
\arguments{
  \item{nfields}{number of fields to simulate (one for each
  basis function)}

  \item{grid.xlim}{vector of length 2 specifying the xlim
  for the grid. Only used if locs is not specified.}

  \item{grid.ylim}{vector of length 2 specifying the ylim
  for the grid. Only used if locs is not specified.}

  \item{grid.dim}{vector with 2 numbers specifying the
  dimension of the rectangular grid. Only used if locs is
  not specified.}

  \item{cov.model}{name of covariance model(s) connected to
  each basis function.}

  \item{cov.pars}{a vector with 2 elements or an n x 2
  matrix with values of the covariance parameters sigma^2
  (partial sill) and phi (range parameter). If a vector,
  the elements are the values of sigma^2 and phi,
  respectively. If a matrix, corresponding to a model with
  several structures, the values of sigma^2 are in the
  first column and the values of phi are in the second.}

  \item{locs}{nx2 matrix of locations}

  \item{...}{other parameters sent to grf()}
}
\value{
coef list containing the coefficients ...

locs nx2 matrix of locations

cov.model name of covariance model(s) connected to each
basis function.

cov.pars covariance parameters corresponding to the
covariance models.
}
\description{
simulates a Gaussian random field for each basis function
given user specified covariance function and covariance
parameters. The model specification corresponds to GeoR
package.
}

